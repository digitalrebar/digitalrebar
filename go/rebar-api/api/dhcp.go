package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes/dhcp"

type dhcpSrc struct{}

func (o *dhcpSrc) serviceSrc() string {
	return "dhcp-mgmt"
}

func (o *dhcpSrc) pathPrefix(trusted bool) string {
	if trusted {
		return ""
	}
	return "/dhcp"
}

type DhcpSubnet struct {
	dhcp.Subnet
	apiHelper
	dhcpSrc
}
