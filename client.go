package crowbar

// Apache 2 License 2015 by Rob Hirschfeld for RackN portions of
// source based on
// https://code.google.com/p/mlab-ns2/source/browse/gae/ns/digest/digest.go
import (
	"bytes"
	"crypto/md5"
	"crypto/rand"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"path"
	"strings"

	"github.com/VictorLowther/jsonpatch"
)

type challenge struct {
	Username   string
	Password   string
	Realm      string
	CSRFToken  string
	Domain     string
	Nonce      string
	Opaque     string
	Stale      string
	Algorithm  string
	Qop        string
	Cnonce     string
	NonceCount int
}

type ocbClient struct {
	*http.Client
	Challenge *challenge
	URL       string
}

// The Crowbar API is exposed over a digest authenticated HTTP(s)
// connection.  This file implements all of the basic REST and HTTP
// operations that Crowbar uses.

// OCB assumes global session created with NewClient
var session *ocbClient

const (
	// The Crowbar API to call.  This will be prepended to every
	// url passed to one of the request functions.
	API_PATH = "/api/v2"
)

// Session establishes a new connection to Crowbar.  You must call
// this function before using any other functions in the crowbar
// package.  Session stores its information in a private global variable.
func Session(URL, User, Password string) error {
	c := &ocbClient{URL: URL, Client: &http.Client{}, Challenge: &challenge{}}
	// retrieve the digest info from the 301 message
	resp, e := c.Head(c.URL + path.Join(API_PATH, "digest"))
	if e != nil {
		return e
	}
	if resp.StatusCode != 401 {
		return fmt.Errorf("Expected Digest Challenge Missing on URL %s got %s", URL, resp.Status)
	}
	var err error
	err = c.Challenge.parseChallenge(resp.Header.Get("WWW-Authenticate"))
	if err != nil {
		return err
	}
	c.Challenge.Username = User
	c.Challenge.Password = Password
	session = c
	return nil
}

func (c *ocbClient) basicRequest(method, uri string, objIn []byte) (resp *http.Response, err error) {
	var body io.Reader

	if objIn != nil {
		body = bytes.NewReader(objIn)
	}
	// Construct the http.Request.
	req, err := http.NewRequest(method, c.URL+path.Join(API_PATH, uri), body)
	if err != nil {
		return nil, err
	}
	auth, err := c.Challenge.authorize(method, path.Join(API_PATH, uri))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Authorization", auth)
	if method == "PATCH" {
		req.Header.Set("Content-Type", jsonpatch.ContentType)
	} else {
		req.Header.Set("Content-Type", "application/json")
	}
	req.Header.Set("User-Agent", "gobar/v1.0")
	req.Header.Set("Accept", "application/json")
	resp, err = c.Do(req)
	if err == nil {
		return
	}
	if resp != nil {
		resp.Body.Close()
	}
	return nil, err
}

// Request makes a general call to the Crowbar API.
// method is the raw HTTP method to use
// uri is the section of the API to call.
// objIn is the raw data to be passed in the request body
// objOut is the raw request body (if any)
// err is the error of any occurred.
func (c *ocbClient) request(method, uri string, objIn []byte) (objOut []byte, err error) {
	resp, err := c.basicRequest(method, uri, objIn)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode == 401 {
		err = c.Challenge.parseChallenge(resp.Header.Get("WWW-Authenticate"))
		if err != nil {
			return nil, err
		}
		resp, err = c.basicRequest(method, uri, objIn)
		if err != nil {
			return nil, err
		}
	}
	objOut, err = ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode >= 300 {
		return nil, fmt.Errorf("Expected status in the 200 range, got %s", resp.Status)
	}
	return objOut, nil
}

// list is a helper specialized to get lists of objects.
func (c *ocbClient) list(res interface{}, uri ...string) (err error) {
	buf, err := c.request("GET", path.Join(uri...), nil)
	if err != nil {
		return err
	}
	return json.Unmarshal(buf, &res)
}

func (c *ocbClient) match(sample interface{}, res interface{}, uri ...string) (err error) {
	inbuf, err := json.Marshal(sample)
	buf, err := c.request("POST", path.Join(uri...), inbuf)
	if err != nil {
		return nil
	}
	return json.Unmarshal(buf, &res)
}

func h(data string) string {
	hf := md5.New()
	io.WriteString(hf, data)
	return fmt.Sprintf("%x", hf.Sum(nil))
}

func kd(secret, data string) string {
	return h(fmt.Sprintf("%s:%s", secret, data))
}

func (c *challenge) ha1() string {
	return h(fmt.Sprintf("%s:%s:%s", c.Username, c.Realm, c.Password))
}

func (c *challenge) ha2(method, uri string) string {
	return h(fmt.Sprintf("%s:%s", method, uri))
}

func (c *challenge) resp(method, uri, cnonce string) (string, error) {
	c.NonceCount++
	if c.Qop == "auth" {
		if cnonce != "" {
			c.Cnonce = cnonce
		} else {
			b := make([]byte, 8)
			io.ReadFull(rand.Reader, b)
			c.Cnonce = fmt.Sprintf("%x", b)[:16]
		}
		return kd(c.ha1(), fmt.Sprintf("%s:%08x:%s:%s:%s",
			c.Nonce, c.NonceCount, c.Cnonce, c.Qop, c.ha2(method, uri))), nil
	} else if c.Qop == "" {
		return kd(c.ha1(), fmt.Sprintf("%s:%s", c.Nonce, c.ha2(method, uri))), nil
	}
	return "", fmt.Errorf("Alg not implemented")
}

// source https://code.google.com/p/mlab-ns2/source/browse/gae/ns/digest/digest.go#178
func (c *challenge) authorize(method, uri string) (string, error) {
	// Note that this is only implemented for MD5 and NOT MD5-sess.
	// MD5-sess is rarely supported and those that do are a big mess.
	if c.Algorithm != "MD5" {
		return "", fmt.Errorf("Alg not implemented")
	}
	// Note that this is NOT implemented for "qop=auth-int".  Similarly the
	// auth-int server side implementations that do exist are a mess.
	if c.Qop != "auth" && c.Qop != "" {
		return "", fmt.Errorf("Alg not implemented")
	}
	resp, err := c.resp(method, uri, "")
	if err != nil {
		return "", fmt.Errorf("Alg not implemented")
	}
	sl := []string{fmt.Sprintf(`username="%s"`, c.Username)}
	sl = append(sl, fmt.Sprintf(`realm="%s"`, c.Realm))
	sl = append(sl, fmt.Sprintf(`nonce="%s"`, c.Nonce))
	sl = append(sl, fmt.Sprintf(`uri="%s"`, uri))
	sl = append(sl, fmt.Sprintf(`response="%s"`, resp))
	if c.Algorithm != "" {
		sl = append(sl, fmt.Sprintf(`algorithm="%s"`, c.Algorithm))
	}
	if c.Opaque != "" {
		sl = append(sl, fmt.Sprintf(`opaque="%s"`, c.Opaque))
	}
	if c.Qop != "" {
		sl = append(sl, fmt.Sprintf("qop=%s", c.Qop))
		sl = append(sl, fmt.Sprintf("nc=%08x", c.NonceCount))
		sl = append(sl, fmt.Sprintf(`cnonce="%s"`, c.Cnonce))
	}
	return fmt.Sprintf("Digest %s", strings.Join(sl, ", ")), nil
}

// origin https://code.google.com/p/mlab-ns2/source/browse/gae/ns/digest/digest.go#90
func (c *challenge) parseChallenge(input string) error {
	const ws = " \n\r\t"
	const qs = `"`
	s := strings.Trim(input, ws)
	if !strings.HasPrefix(s, "Digest ") {
		return fmt.Errorf("Challenge is bad, missing prefix: %s", input)
	}
	s = strings.Trim(s[7:], ws)
	sl := strings.Split(s, ", ")
	c.Algorithm = "MD5"
	var r []string
	for i := range sl {
		r = strings.SplitN(sl[i], "=", 2)
		switch r[0] {
		case "realm":
			c.Realm = strings.Trim(r[1], qs)
		case "domain":
			c.Domain = strings.Trim(r[1], qs)
		case "nonce":
			c.Nonce = strings.Trim(r[1], qs)
		case "opaque":
			c.Opaque = strings.Trim(r[1], qs)
		case "stale":
			c.Stale = strings.Trim(r[1], qs)
		case "algorithm":
			c.Algorithm = strings.Trim(r[1], qs)
		case "qop":
			//TODO(gavaletz) should be an array of strings?
			c.Qop = strings.Trim(r[1], qs)
		default:
			return fmt.Errorf("Challenge is bad, unexpected token: %s", sl)
		}
	}
	return nil
}
