package main

import (
	"encoding/binary"
	"errors"
	"fmt"
	dhcp "github.com/krolaw/dhcp4"
	"github.com/willf/bitset"
	"net"
	"strconv"
	"strings"
	"time"
)

func convertByteToOptionValue(code dhcp.OptionCode, b []byte) string {
	switch code {
	// Single IP-like address
	case dhcp.OptionSubnetMask,
		dhcp.OptionBroadcastAddress,
		dhcp.OptionSwapServer,
		dhcp.OptionRouterSolicitationAddress,
		dhcp.OptionRequestedIPAddress,
		dhcp.OptionServerIdentifier:
		return net.IP(b).To4().String()

	// Multiple IP-like address
	case dhcp.OptionRouter,
		dhcp.OptionTimeServer,
		dhcp.OptionNameServer,
		dhcp.OptionDomainNameServer,
		dhcp.OptionLogServer,
		dhcp.OptionCookieServer,
		dhcp.OptionLPRServer,
		dhcp.OptionImpressServer,
		dhcp.OptionResourceLocationServer,
		dhcp.OptionPolicyFilter, // This is special and could validate more (2Ips per)
		dhcp.OptionStaticRoute,  // This is special and could validate more (2IPs per)
		dhcp.OptionNetworkInformationServers,
		dhcp.OptionNetworkTimeProtocolServers,
		dhcp.OptionNetBIOSOverTCPIPNameServer,
		dhcp.OptionNetBIOSOverTCPIPDatagramDistributionServer,
		dhcp.OptionXWindowSystemFontServer,
		dhcp.OptionXWindowSystemDisplayManager,
		dhcp.OptionNetworkInformationServicePlusServers,
		dhcp.OptionMobileIPHomeAgent,
		dhcp.OptionSimpleMailTransportProtocol,
		dhcp.OptionPostOfficeProtocolServer,
		dhcp.OptionNetworkNewsTransportProtocol,
		dhcp.OptionDefaultWorldWideWebServer,
		dhcp.OptionDefaultFingerServer,
		dhcp.OptionDefaultInternetRelayChatServer,
		dhcp.OptionStreetTalkServer,
		dhcp.OptionStreetTalkDirectoryAssistance:

		addrs := make([]string, 0)
		for len(b) > 0 {
			addrs = append(addrs, net.IP(b[0:4]).To4().String())
			b = b[4:]
		}
		return strings.Join(addrs, ",")

	// String like value
	case dhcp.OptionHostName,
		dhcp.OptionMeritDumpFile,
		dhcp.OptionDomainName,
		dhcp.OptionRootPath,
		dhcp.OptionExtensionsPath,
		dhcp.OptionNetworkInformationServiceDomain,
		dhcp.OptionVendorSpecificInformation, // This is wrong, but ...
		dhcp.OptionNetBIOSOverTCPIPScope,
		dhcp.OptionNetworkInformationServicePlusDomain,
		dhcp.OptionTFTPServerName,
		dhcp.OptionBootFileName,
		dhcp.OptionMessage,
		dhcp.OptionVendorClassIdentifier,
		dhcp.OptionClientIdentifier,
		dhcp.OptionUserClass,
		dhcp.OptionTZPOSIXString,
		dhcp.OptionTZDatabaseString:
		return string(b[:len(b)])

	// 4 byte integer value
	case dhcp.OptionTimeOffset,
		dhcp.OptionPathMTUAgingTimeout,
		dhcp.OptionARPCacheTimeout,
		dhcp.OptionTCPKeepaliveInterval,
		dhcp.OptionIPAddressLeaseTime,
		dhcp.OptionRenewalTimeValue,
		dhcp.OptionRebindingTimeValue:
		return fmt.Sprint(binary.BigEndian.Uint32(b))

	// 2 byte integer value
	case dhcp.OptionBootFileSize,
		dhcp.OptionMaximumDatagramReassemblySize,
		dhcp.OptionInterfaceMTU,
		dhcp.OptionMaximumDHCPMessageSize:
		return fmt.Sprint(binary.BigEndian.Uint16(b))

	// 1 byte integer value
	case dhcp.OptionIPForwardingEnableDisable,
		dhcp.OptionNonLocalSourceRoutingEnableDisable,
		dhcp.OptionDefaultIPTimeToLive,
		dhcp.OptionAllSubnetsAreLocal,
		dhcp.OptionPerformMaskDiscovery,
		dhcp.OptionMaskSupplier,
		dhcp.OptionPerformRouterDiscovery,
		dhcp.OptionTrailerEncapsulation,
		dhcp.OptionEthernetEncapsulation,
		dhcp.OptionTCPDefaultTTL,
		dhcp.OptionTCPKeepaliveGarbage,
		dhcp.OptionNetBIOSOverTCPIPNodeType,
		dhcp.OptionOverload,
		dhcp.OptionDHCPMessageType:
		return fmt.Sprint(b[0])

		// Empty
	case dhcp.Pad, dhcp.End:
		return ""
	}

	return ""
}

func convertOptionValueToByte(code dhcp.OptionCode, value string) ([]byte, error) {
	switch code {
	// Single IP-like address
	case dhcp.OptionSubnetMask,
		dhcp.OptionBroadcastAddress,
		dhcp.OptionSwapServer,
		dhcp.OptionRouterSolicitationAddress,
		dhcp.OptionRequestedIPAddress,
		dhcp.OptionServerIdentifier:
		return []byte(net.ParseIP(value).To4()), nil

	// Multiple IP-like address
	case dhcp.OptionRouter,
		dhcp.OptionTimeServer,
		dhcp.OptionNameServer,
		dhcp.OptionDomainNameServer,
		dhcp.OptionLogServer,
		dhcp.OptionCookieServer,
		dhcp.OptionLPRServer,
		dhcp.OptionImpressServer,
		dhcp.OptionResourceLocationServer,
		dhcp.OptionPolicyFilter, // This is special and could validate more (2Ips per)
		dhcp.OptionStaticRoute,  // This is special and could validate more (2IPs per)
		dhcp.OptionNetworkInformationServers,
		dhcp.OptionNetworkTimeProtocolServers,
		dhcp.OptionNetBIOSOverTCPIPNameServer,
		dhcp.OptionNetBIOSOverTCPIPDatagramDistributionServer,
		dhcp.OptionXWindowSystemFontServer,
		dhcp.OptionXWindowSystemDisplayManager,
		dhcp.OptionNetworkInformationServicePlusServers,
		dhcp.OptionMobileIPHomeAgent,
		dhcp.OptionSimpleMailTransportProtocol,
		dhcp.OptionPostOfficeProtocolServer,
		dhcp.OptionNetworkNewsTransportProtocol,
		dhcp.OptionDefaultWorldWideWebServer,
		dhcp.OptionDefaultFingerServer,
		dhcp.OptionDefaultInternetRelayChatServer,
		dhcp.OptionStreetTalkServer,
		dhcp.OptionStreetTalkDirectoryAssistance:

		addrs := make([]net.IP, 0)
		alist := strings.Split(value, ",")
		for i := range alist {
			addrs = append(addrs, net.ParseIP(alist[i]).To4())
		}
		return dhcp.JoinIPs(addrs), nil

	// String like value
	case dhcp.OptionHostName,
		dhcp.OptionMeritDumpFile,
		dhcp.OptionDomainName,
		dhcp.OptionRootPath,
		dhcp.OptionExtensionsPath,
		dhcp.OptionNetworkInformationServiceDomain,
		dhcp.OptionVendorSpecificInformation, // This is wrong, but ...
		dhcp.OptionNetBIOSOverTCPIPScope,
		dhcp.OptionNetworkInformationServicePlusDomain,
		dhcp.OptionTFTPServerName,
		dhcp.OptionBootFileName,
		dhcp.OptionMessage,
		dhcp.OptionVendorClassIdentifier,
		dhcp.OptionClientIdentifier,
		dhcp.OptionUserClass,
		dhcp.OptionTZPOSIXString,
		dhcp.OptionTZDatabaseString:
		return []byte(value), nil

	// 4 byte integer value
	case dhcp.OptionTimeOffset,
		dhcp.OptionPathMTUAgingTimeout,
		dhcp.OptionARPCacheTimeout,
		dhcp.OptionTCPKeepaliveInterval,
		dhcp.OptionIPAddressLeaseTime,
		dhcp.OptionRenewalTimeValue,
		dhcp.OptionRebindingTimeValue:
		answer := make([]byte, 4)
		ival, err := strconv.Atoi(value)
		if err != nil {
			return nil, err
		}
		binary.BigEndian.PutUint32(answer, uint32(ival))
		return answer, nil

	// 2 byte integer value
	case dhcp.OptionBootFileSize,
		dhcp.OptionMaximumDatagramReassemblySize,
		dhcp.OptionInterfaceMTU,
		dhcp.OptionMaximumDHCPMessageSize:
		answer := make([]byte, 2)
		ival, err := strconv.Atoi(value)
		if err != nil {
			return nil, err
		}
		binary.BigEndian.PutUint16(answer, uint16(ival))
		return answer, nil

	// 1 byte integer value
	case dhcp.OptionIPForwardingEnableDisable,
		dhcp.OptionNonLocalSourceRoutingEnableDisable,
		dhcp.OptionDefaultIPTimeToLive,
		dhcp.OptionAllSubnetsAreLocal,
		dhcp.OptionPerformMaskDiscovery,
		dhcp.OptionMaskSupplier,
		dhcp.OptionPerformRouterDiscovery,
		dhcp.OptionTrailerEncapsulation,
		dhcp.OptionEthernetEncapsulation,
		dhcp.OptionTCPDefaultTTL,
		dhcp.OptionTCPKeepaliveGarbage,
		dhcp.OptionNetBIOSOverTCPIPNodeType,
		dhcp.OptionOverload,
		dhcp.OptionDHCPMessageType:
		answer := make([]byte, 1)
		ival, err := strconv.Atoi(value)
		if err != nil {
			return nil, err
		}
		answer[0] = byte(ival)
		return answer, nil

		// Empty
	case dhcp.Pad, dhcp.End:
		return make([]byte, 0), nil
	}

	return nil, errors.New("Invalid Option: " + code.String() + " " + value)
}

// Others in the dhcp library, but not likely needed for input.
//
// Array of 2-byte integers
// OptionPathMTUPlateauTable                OptionCode = 25
// OptionClientArchitecture OptionCode = 93
// Array of 1-byte integers
//OptionParameterRequestList   OptionCode = 55
// Complex See RFC 3046
// OptionRelayAgentInformation OptionCode = 82
// OptionClasslessRouteFormat OptionCode = 121

func convertSubnetToApiSubnet(s *Subnet) *ApiSubnet {
	apiSubnet := NewApiSubnet()
	apiSubnet.Name = s.Name
	apiSubnet.Subnet = s.Subnet.String()
	apiSubnet.ActiveStart = s.ActiveStart.String()
	apiSubnet.ActiveEnd = s.ActiveEnd.String()
	apiSubnet.ActiveLeaseTime = int(s.ActiveLeaseTime.Seconds())
	apiSubnet.ReservedLeaseTime = int(s.ReservedLeaseTime.Seconds())

	if s.NextServer != nil {
		ns := s.NextServer.String()
		apiSubnet.NextServer = &ns
	}

	for _, v := range s.Leases {
		apiSubnet.Leases = append(apiSubnet.Leases, v)
	}

	for _, v := range s.Bindings {
		apiSubnet.Bindings = append(apiSubnet.Bindings, v)
	}

	for i, v := range s.Options {
		o := &Option{
			Code:  i,
			Value: convertByteToOptionValue(i, v),
		}
		apiSubnet.Options = append(apiSubnet.Options, o)
	}

	return apiSubnet
}

func convertApiSubnetToSubnet(as *ApiSubnet, subnet *Subnet) (*Subnet, error) {
	if subnet == nil {
		subnet = NewSubnet()
	}
	subnet.Name = as.Name

	_, netdata, err := net.ParseCIDR(as.Subnet)
	if err != nil {
		return nil, err
	}

	subnet.Subnet = &MyIPNet{netdata}
	subnet.ActiveStart = net.ParseIP(as.ActiveStart).To4()
	subnet.ActiveEnd = net.ParseIP(as.ActiveEnd).To4()
	subnet.ActiveLeaseTime = time.Duration(as.ActiveLeaseTime) * time.Second
	subnet.ReservedLeaseTime = time.Duration(as.ReservedLeaseTime) * time.Second
	subnet.ActiveBits = bitset.New(uint(dhcp.IPRange(subnet.ActiveStart, subnet.ActiveEnd)))

	if as.NextServer != nil {
		ip := net.ParseIP(*as.NextServer).To4()
		subnet.NextServer = &ip
	}

	if subnet.ActiveLeaseTime == 0 {
		subnet.ActiveLeaseTime = 30 * time.Second
	}
	if subnet.ReservedLeaseTime == 0 {
		subnet.ReservedLeaseTime = 2 * time.Hour
	}

	for _, v := range as.Leases {
		subnet.Leases[v.Mac] = v
		if dhcp.IPInRange(subnet.ActiveStart, subnet.ActiveEnd, v.Ip) {
			subnet.ActiveBits.Set(uint(dhcp.IPRange(subnet.ActiveStart, v.Ip) - 1))
		}
	}

	for _, v := range as.Bindings {
		subnet.Bindings[v.Mac] = v
		if dhcp.IPInRange(subnet.ActiveStart, subnet.ActiveEnd, v.Ip) {
			subnet.ActiveBits.Set(uint(dhcp.IPRange(subnet.ActiveStart, v.Ip) - 1))
		}
	}

	// Seed initial options
	subnet.Options[dhcp.OptionSubnetMask] = []byte(net.IP(netdata.Mask).To4())
	m := binary.BigEndian.Uint32(subnet.Options[dhcp.OptionSubnetMask])
	n := binary.BigEndian.Uint32(netdata.IP)
	b := n | ^m
	result := make([]byte, 4)
	binary.BigEndian.PutUint32(result, b)
	subnet.Options[dhcp.OptionBroadcastAddress] = result

	for _, o := range as.Options {
		subnet.Options[o.Code], err = convertOptionValueToByte(o.Code, o.Value)
		if err != nil {
			return nil, err
		}
	}

	if !netdata.Contains(subnet.ActiveStart) {
		return nil, errors.New("ActiveStart not in Subnet")
	}
	if !netdata.Contains(subnet.ActiveEnd) {
		return nil, errors.New("ActiveEnd not in Subnet")
	}

	if dhcp.IPLess(subnet.ActiveEnd, subnet.ActiveStart) {
		return nil, errors.New("ActiveEnd less than ActiveStart")
	}

	return subnet, nil
}
