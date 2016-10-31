package api

import (
	"encoding/json"
	"fmt"
	"net"
)

func Whoami(c *Client) (*Node, error) {
	res, err := localWhoami(c)
	if res != nil || err != nil {
		return res, err
	}

	type finder struct {
		Addrs []string `json:"addrs"`
		Macs  []string `json:"macs"`
	}
	ifaces, err := net.Interfaces()
	if err != nil {
		return nil, fmt.Errorf("Error getting interfaces: %v", err)
	}
	addrs, err := net.InterfaceAddrs()
	if err != nil {
		return nil, fmt.Errorf("Error getting addresses: %v", err)
	}
	info := &finder{Addrs: make([]string, len(addrs)), Macs: make([]string, len(ifaces))}
	for i, iface := range ifaces {
		info.Macs[i] = iface.HardwareAddr.String()
	}
	for i, addr := range addrs {
		info.Addrs[i] = addr.String()
	}
	findBlock, err := json.Marshal(info)
	if err != nil {
		return nil, fmt.Errorf("Error marshalling finder block: %v", err)
	}
	res = &Node{}
	url := c.UrlPath(res, "whoami")
	buf, err := c.request("POST", url, findBlock)
	if err != nil {
		return nil, err
	}
	return res, c.unmarshal(url, buf, res)
}
