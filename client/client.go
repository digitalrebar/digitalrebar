package client

// Apache 2 License 2015 by Rob Hirschfeld for RackN portions of
// source based on
// https://code.google.com/p/mlab-ns2/source/browse/gae/ns/digest/digest.go
import (
	"bytes"
	"crypto/md5"
	"crypto/rand"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"path"
	"strings"
	"text/template"

	"github.com/VictorLowther/jsonpatch"
	"golang.org/x/net/html"
)

type challengeDigest struct {
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

type challengeSAML struct {
	Username string
	Password string
	Token    string
}

type challenge interface {
	parseChallenge(resp *http.Response) error
	authorize(method, uri string, req *http.Request) error
}

type rebarClient struct {
	*http.Client
	Challenge challenge
	URL       string
}

// The Rebar API is exposed over a digest authenticated HTTP(s)
// connection.  This file implements all of the basic REST and HTTP
// operations that Rebar uses.

// OCB assumes global session created with NewClient
var session *rebarClient

const (
	// The Rebar API to call.  This will be prepended to every
	// url passed to one of the request functions.
	API_PATH = "/api/v2"
)

// Session establishes a new connection to Rebar.  You must call
// this function before using any other functions in the rebar
// package.  Session stores its information in a private global variable.
func Session(URL, User, Password string) error {
	tr := &http.Transport{
		TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
	}
	c := &rebarClient{
		URL:    URL,
		Client: &http.Client{Transport: tr},
	}
	// retrieve the digest info from the 301 message
	resp, e := c.Head(c.URL + path.Join(API_PATH, "digest"))
	if e != nil {
		return e
	}
	if resp.StatusCode != 401 && resp.StatusCode != 200 {
		return fmt.Errorf("Expected Digest Challenge os SAML Redirect Missing on URL %s got %s", URL, resp.Status)
	}

	// We may be SAML Auth
	if resp.StatusCode == 200 {
		c.Challenge = &challengeSAML{
			Username: User,
			Password: Password,
		}
	} else {
		c.Challenge = &challengeDigest{
			Username: User,
			Password: Password,
		}
	}

	err := c.Challenge.parseChallenge(resp)
	if err != nil {
		return err
	}
	session = c
	return nil
}

func (c *rebarClient) basicRequest(method, uri string, objIn []byte) (resp *http.Response, err error) {
	var body io.Reader

	if objIn != nil {
		body = bytes.NewReader(objIn)
	}
	// Construct the http.Request.
	req, err := http.NewRequest(method, c.URL+path.Join(API_PATH, uri), body)
	if err != nil {
		return nil, err
	}
	err = c.Challenge.authorize(method, path.Join(API_PATH, uri), req)
	if err != nil {
		return nil, err
	}
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

// Request makes a general call to the Rebar API.
// method is the raw HTTP method to use
// uri is the section of the API to call.
// objIn is the raw data to be passed in the request body
// objOut is the raw request body (if any)
// err is the error of any occurred.
func (c *rebarClient) request(method, uri string, objIn []byte) (objOut []byte, err error) {
	resp, err := c.basicRequest(method, uri, objIn)
	if err != nil {
		return nil, err
	}
	if resp.Body != nil {
		defer resp.Body.Close()
	}

	if resp.StatusCode == 401 {
		err = c.Challenge.parseChallenge(resp)
		if err != nil {
			return nil, err
		}
		resp, err = c.basicRequest(method, uri, objIn)
		if err != nil {
			return nil, err
		}
		if resp.Body != nil {
			defer resp.Body.Close()
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
func (c *rebarClient) list(res interface{}, uri ...string) (err error) {
	buf, err := c.request("GET", path.Join(uri...), nil)
	if err != nil {
		return err
	}
	return json.Unmarshal(buf, &res)
}

func (c *rebarClient) match(vals map[string]interface{}, res interface{}, uri ...string) (err error) {
	inbuf, err := json.Marshal(vals)
	buf, err := c.request("POST",
		path.Join(path.Join(uri...), "match"),
		inbuf)
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

func (c *challengeDigest) ha1() string {
	return h(fmt.Sprintf("%s:%s:%s", c.Username, c.Realm, c.Password))
}

func (c *challengeDigest) ha2(method, uri string) string {
	return h(fmt.Sprintf("%s:%s", method, uri))
}

func (c *challengeDigest) resp(method, uri, cnonce string) (string, error) {
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
func (c *challengeDigest) authorize(method, uri string, req *http.Request) error {
	// Note that this is only implemented for MD5 and NOT MD5-sess.
	// MD5-sess is rarely supported and those that do are a big mess.
	if c.Algorithm != "MD5" {
		return fmt.Errorf("Alg not implemented")
	}
	// Note that this is NOT implemented for "qop=auth-int".  Similarly the
	// auth-int server side implementations that do exist are a mess.
	if c.Qop != "auth" && c.Qop != "" {
		return fmt.Errorf("Alg not implemented")
	}
	resp, err := c.resp(method, uri, "")
	if err != nil {
		return fmt.Errorf("Alg not implemented")
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
	req.Header.Set("Authorization", fmt.Sprintf("Digest %s", strings.Join(sl, ", ")))
	return nil
}

// origin https://code.google.com/p/mlab-ns2/source/browse/gae/ns/digest/digest.go#90
func (c *challengeDigest) parseChallenge(resp *http.Response) error {
	input := resp.Header.Get("WWW-Authenticate")
	const ws = " \n\r\t"
	const qs = `"`
	s := strings.Trim(input, ws)
	if !strings.HasPrefix(s, "Digest ") {
		return fmt.Errorf("Challenge is bad, missing prefix: %s", input)
	}
	s = strings.Trim(s[7:], ws)
	sl := strings.Split(s, ",")
	c.Algorithm = "MD5"
	var r []string
	for i := range sl {
		r = strings.SplitN(sl[i], "=", 2)
		switch strings.TrimSpace(r[0]) {
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

type SamlCookieRequest struct {
	Status       string
	SessionToken string
}

func (c *challengeSAML) parseChallenge(resp *http.Response) error {
	b64req := resp.Header.Get("DR-SAML-REQ")
	samlurl := resp.Header.Get("DR-SAML-URL")

	if b64req == "" || samlurl == "" {
		return fmt.Errorf("Expected SAML challenge/assertion, but was missing")
	}

	surl, err := url.Parse(samlurl)
	if err != nil {
		fmt.Println("Failed to parse samlurl", err)
		return err
	}

	// Using creds, get one-time cookie token.
	surl.Path = "/api/v1/authn"
	tr := &http.Transport{
		TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
	}
	hc := &http.Client{Transport: tr}
	t := template.New("saml-auth")
	t, err = t.Parse("{ \"username\": \"{{.Username}}\", \"password\": \"{{.Password}}\", \"relayState\": \"/myapp/some/deep/link/i/want/to/return/to\", \"options\": { \"multiOptionalFactorEnroll\": false, \"warnBeforePasswordExpired\": false } }")
	if err != nil {
		fmt.Println("Failed to generate template", err)
		return err
	}
	var doc bytes.Buffer
	err = t.Execute(&doc, c)
	if err != nil {
		fmt.Println("Failed to execute template", err)
		return err
	}

	auth_resp, err := hc.Post(surl.String(), "application/json", &doc)
	if err != nil {
		fmt.Println("Failed to get auth cookie", err)
		return err
	}
	defer auth_resp.Body.Close()

	objOut, err := ioutil.ReadAll(auth_resp.Body)
	if err != nil {
		fmt.Println("Failed to read get auth cookie body", err)
		return err
	}
	var cookieData SamlCookieRequest
	err = json.Unmarshal(objOut, &cookieData)
	if err != nil {
		fmt.Println("Failed to parse json return", err)
		return err
	}

	// With one-time cookie token, validate the saml assertion
	samlurl = samlurl + "&onetimetoken=" + cookieData.SessionToken
	auth_request, err := http.NewRequest("PUT", samlurl, strings.NewReader(""))
	auth_request.ContentLength = 0
	auth_resp, err = hc.Do(auth_request)
	if err != nil {
		fmt.Println("Failed to auth with cookie")
		return err
	}
	defer auth_resp.Body.Close()

	url, form_data, err := find_form_data(auth_resp.Body)
	if err != nil {
		fmt.Println(err)
		return err
	}

	// Send response to DR
	dr_auth_resp, err := hc.PostForm(*url, form_data)
	if err != nil {
		fmt.Println(err)
		return err
	}
	// Don't need body, just headers
	if dr_auth_resp.Body != nil {
		dr_auth_resp.Body.Close()
	}

	// Get redirect - DONE - record token.
	if dr_auth_resp.StatusCode != 301 {
		return fmt.Errorf("Should have received a redirect, but got %v", dr_auth_resp.StatusCode)
	}

	// Finally got the auth token!!
	c.Token = dr_auth_resp.Header.Get("Dr-Auth-Token")

	return nil
}

func find_form_data(r io.Reader) (*string, url.Values, error) {
	doc, err := html.Parse(r)
	if err != nil {
		return nil, nil, err
	}

	// Find first form
	var f func(n *html.Node) *html.Node
	f = func(n *html.Node) *html.Node {
		if n.Type == html.ElementNode && n.Data == "form" {
			return n
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			v := f(c)
			if v != nil {
				return v
			}
		}
		return nil
	}
	form := f(doc)
	if form == nil {
		return nil, nil, fmt.Errorf("Expected form, but non found")
	}

	inputs := make(url.Values, 0)
	f = func(n *html.Node) *html.Node {
		if n.Type == html.ElementNode && n.Data == "input" {
			var name string
			var data string

			for _, a := range n.Attr {
				if a.Key == "name" {
					name = a.Val
				} else if a.Key == "value" {
					data = a.Val
				}
			}

			inputs[name] = []string{data}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
		return nil
	}
	f(form)

	var url string
	for _, a := range form.Attr {
		if a.Key == "action" {
			url = a.Val
		}
	}

	return &url, inputs, nil
}

func (c *challengeSAML) authorize(method, uri string, req *http.Request) error {
	req.Header.Set("DR-AUTH-TOKEN", c.Token)
	req.Header.Set("DR-AUTH-USER", c.Username)
	return nil
}
