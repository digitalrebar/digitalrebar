package api

import "net/http"

type challengeTrusted string

// No need to handle parsing the challenge, Rebar
// will believe we are who we say we are.
func (c challengeTrusted) parseChallenge(resp *http.Response) error {
	return nil
}

// This only works from inside the trust boundary at rev-proxy
func (c challengeTrusted) authorize(method, uri string, req *http.Request) error {
	req.Header.Set("X-Authenticated-Username", string(c))
	return nil
}
