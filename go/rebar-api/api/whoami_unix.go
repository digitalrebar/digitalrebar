// +build !windows

package api

import (
	"bytes"
	"io/ioutil"
)

func localWhoami(c *Client) (*Node, error) {
	buf, err := ioutil.ReadFile("/etc/rebar-uuid")
	if err != nil {
		return nil, nil
	}
	res := &Node{}
	res.UUID = string(bytes.TrimSpace(buf))
	return res, c.Read(res)
}
