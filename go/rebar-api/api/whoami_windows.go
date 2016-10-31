// +build windows

package api

import (
	"bytes"
	"io/ioutil"
	"os"
	"path/filepath"
)

func localWhoami(c *Client) (*Node, error) {
	file := filepath.Join(os.Getenv("SYSTEMDRIVE"), "rebar-uuid")
	buf, err := ioutil.ReadFile(file)
	if err != nil {
		return nil, nil
	}
	res := &Node{}
	res.UUID = string(bytes.TrimSpace(buf))
	return res, c.Read(res)
}
