package api

import (
	"encoding/json"
	"fmt"
	"net/http"
)

type challengeTrusted struct {
	username string
	caps     *map[string]interface{}
}

// No need to handle parsing the challenge, Rebar
// will believe we are who we say we are.
func (c *challengeTrusted) parseChallenge(resp *http.Response) error {
	return nil
}

// This only works from inside the trust boundary at rev-proxy
func (c *challengeTrusted) authorize(req *http.Request) error {
	req.Header.Set("X-Authenticated-Username", c.username)
	if c.caps != nil {
		buf, err := json.Marshal(c.caps)
		if err != nil {
			return fmt.Errorf("Failed to marshal caps: %v", err)
		}
		req.Header.Set("X-Authenticated-Capability", string(buf))
	}
	return nil
}
