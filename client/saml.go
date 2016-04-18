package client

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"
	"text/template"

	"golang.org/x/net/html"
)

type challengeSAML struct {
	Username string
	Password string
	Token    string
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
