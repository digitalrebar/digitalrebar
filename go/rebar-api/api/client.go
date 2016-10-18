package api

// Apache 2 License 2015 by Rob Hirschfeld for RackN portions of
// source based on
// https://code.google.com/p/mlab-ns2/source/browse/gae/ns/digest/digest.go
import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"path"
	"sync"
	"time"

	"github.com/VictorLowther/jsonpatch"
	"github.com/digitalrebar/digitalrebar/go/common/cert"
	"github.com/digitalrebar/go-common/client"
	"github.com/digitalrebar/go-common/service"
)

type challenge interface {
	parseChallenge(resp *http.Response) error
	authorize(req *http.Request) error
}

// Client wraps http.Client to add our auth primitives.
type Client struct {
	*http.Client
	Challenge    challenge
	URL          string
	trusted      bool
	objPathCache map[string]string
	ocMutex      sync.Mutex
}

func (c *Client) Trusted() bool {
	return c.trusted
}

// UrlFor is the path to the API endpoint that this object came from.
//
// c.UrlFor(foo) -> https://endpoint:port/o.pathPrefix(c.trusted)
func (c *Client) UrlFor(o apiSrc, parts ...string) string {
	uri := c.URL
	if c.trusted {
		c.ocMutex.Lock()
		defer c.ocMutex.Unlock()
		uri, ok := c.objPathCache[o.serviceSrc()]
		if !ok {
			cClient, err := client.Consul(true)
			if err != nil {
				log.Fatalf("Unable to talk to Consul: %v", err)
			}
			objSrc, err := service.Find(cClient, o.serviceSrc(), "")
			if err != nil {
				log.Fatalf("Cannot find service %s in Consul: %v", o.serviceSrc(), err)
			}
			if len(objSrc) == 0 {
				log.Fatalf("No services registered for %s: %v", o.serviceSrc(), err)
			}
			objAddr, objPort := service.Address(objSrc[0])
			uri = fmt.Sprintf("https://%s:%d", objAddr, objPort)
			c.objPathCache[o.serviceSrc()] = uri
		}
	}
	return uri + path.Join(append([]string{"/", o.pathPrefix(c.trusted)}, parts...)...)
}

// UrlPath is the path to the API endpoint for this class of object.
//
// UrlPath(foo) -> https://endpoint:port/o.pathPrefix(c.trusted)/foo.ApiName()
func (c *Client) UrlPath(o Crudder, parts ...string) string {
	return c.UrlFor(o, append([]string{o.ApiName()}, parts...)...)
}

// UrlTo is the path to the API endpoint for a specific object
// UrlPath(foo) -> https://endpoint:port/o.pathPrefix(c.trusted)/foo.ApiName()/foo.Id()
func (c *Client) UrlTo(o Crudder, parts ...string) string {
	id, err := o.Id()
	if err != nil {
		log.Panic(err)
	}
	return c.UrlPath(o, append([]string{id}, parts...)...)
}

// The Rebar API is exposed over a digest authenticated HTTP(s)
// connection.  This file implements all of the basic REST and HTTP
// operations that Rebar uses.

// TrustedSession builds a Client that can only operate inside the local trust zone.
// It assumes that there is a local Consul server that it can use to look up the
// trust-me service and the internal endpoint for the Rebar API.
func TrustedSession(username string, wait bool) (*Client, error) {
	cClient, err := client.Consul(wait)
	if err != nil {
		return nil, err
	}
	for {
		apiService, err := service.Find(cClient, "rebar-api", "")
		if err != nil {
			return nil, fmt.Errorf("RebarClient: Error talking to local Consul: %v", err)
		}
		if len(apiService) == 0 {
			if wait {
				log.Println("Rebar API service not registered yet")
				time.Sleep(10 * time.Second)
				continue
			}
			return nil, fmt.Errorf("RebarClient: Rebar API not registered with Consul")
		}
		apiAddr, apiPort := service.Address(apiService[0])
		c, err := cert.Client("internal", "rebar-client")
		if err != nil {
			return nil, err
		}
		res := &Client{
			URL:          fmt.Sprintf("https://%s:%d", apiAddr, apiPort),
			Client:       c,
			Challenge:    challengeTrusted(username),
			trusted:      true,
			objPathCache: map[string]string{},
		}
		user := &User{}
		if err := res.Fetch(user, username); err != nil {
			return nil, fmt.Errorf("Unable to verify existence of %s user, cannot use trusted session: %v", username, err)
		}
		return res, nil
	}
}

// Session establishes a new connection to Rebar.  You must cal
// this function before using any other functions in the rebar
// package.  Session stores its information in a private global variable.
func Session(URL, User, Password string) (*Client, error) {
	tr := &http.Transport{
		TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
	}
	c := &Client{
		URL:     URL,
		Client:  &http.Client{Transport: tr},
		trusted: false,
	}
	// retrieve the digest info from the 301 message
	rs := rebarSrc{}
	resp, e := c.Head(c.URL + path.Join(rs.pathPrefix(c.trusted), "digest"))
	if e != nil {
		return nil, e
	}
	if resp.StatusCode != 401 && resp.StatusCode != 200 {
		return nil, fmt.Errorf("Expected Digest Challenge os SAML Redirect Missing on URL %s got %s", URL, resp.Status)
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
		return nil, err
	}
	return c, nil
}

func (c *Client) basicRequest(req *http.Request) (resp *http.Response, err error) {
	auth := false
	for {
		err = c.Challenge.authorize(req)
		if err != nil {
			return nil, err
		}
		resp, err = c.Do(req)
		if err != nil || resp.StatusCode != 401 || auth {
			return
		}
		auth = true
		resp.Body.Close()
		err = c.Challenge.parseChallenge(resp)
		if err != nil {
			return nil, err
		}
	}
}

// Request makes a general call to the Rebar API.
// method is the raw HTTP method to use
// uri is the section of the API to call.
// objIn is the raw data to be passed in the request body
// objOut is the raw request body (if any)
// err is the error of any occurred.
func (c *Client) request(method, uri string, objIn []byte) (objOut []byte, err error) {
	var body io.Reader
	if objIn != nil {
		body = bytes.NewReader(objIn)
	}
	req, err := http.NewRequest(method, uri, body)
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
	resp, err := c.basicRequest(req)
	if err != nil {
		return nil, err
	}
	if resp.Body != nil {
		defer resp.Body.Close()
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
func (c *Client) list(res interface{}, uri string) (err error) {
	buf, err := c.request("GET", uri, nil)
	if err != nil {
		return err
	}
	return json.Unmarshal(buf, &res)
}

func (c *Client) match(vals map[string]interface{}, res interface{}, uri string) (err error) {
	inbuf, err := json.Marshal(vals)
	buf, err := c.request("POST", uri+"/match", inbuf)
	if err != nil {
		return nil
	}
	return json.Unmarshal(buf, &res)
}
