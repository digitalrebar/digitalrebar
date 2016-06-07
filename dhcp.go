// Example of minimal DHCP server:
package main

import (
	"log"
	"net"
	"strings"

	dhcp "github.com/krolaw/dhcp4"
)

func RunDhcpHandler(dhcpInfo *DataTracker, intf net.Interface, myIp string) {
	log.Println("Starting on interface: ", intf.Name, " with server ip: ", myIp)

	serverIP, _, _ := net.ParseCIDR(myIp)
	serverIP = serverIP.To4()
	handler := &DHCPHandler{
		ip:   serverIP,
		intf: intf,
		info: dhcpInfo,
	}
	log.Fatal(dhcp.ListenAndServeIf(intf.Name, handler))
}

func StartDhcpHandlers(dhcpInfo *DataTracker, serverIp string) error {
	intfs, err := net.Interfaces()
	if err != nil {
		return err
	}
	for _, intf := range intfs {
		if (intf.Flags & net.FlagLoopback) == net.FlagLoopback {
			continue
		}
		if (intf.Flags & net.FlagUp) != net.FlagUp {
			continue
		}
		if strings.HasPrefix(intf.Name, "veth") {
			continue
		}
		var sip string
		addrs, err := intf.Addrs()
		if err != nil {
			return err
		}

		for _, addr := range addrs {
			thisIP, _, _ := net.ParseCIDR(addr.String())
			// Only care about addresses that are not link-local.
			if !thisIP.IsGlobalUnicast() {
				continue
			}
			// Only deal with IPv4 for now.
			if thisIP.To4() == nil {
				continue
			}

			if serverIp != "" && serverIp == addr.String() {
				sip = addr.String()
				break
			}
		}

		if sip == "" {
			continue
		}
		// Only run the first one that matches
		go RunDhcpHandler(dhcpInfo, intf, sip)
		break
	}
	return nil
}

type DHCPHandler struct {
	intf net.Interface // Interface processing on.
	ip   net.IP        // Server IP to use
	info *DataTracker  // Subnet data
}

func (h *DHCPHandler) ServeDHCP(p dhcp.Packet, msgType dhcp.MessageType, options dhcp.Options) (d dhcp.Packet) {

	// First find the subnet to use. giaddr field to lookup subnet if not all zeros.
	// If all zeros, use the interfaces Addrs to find a subnet, first wins.
	var subnet *Subnet
	subnet = nil

	nic := strings.ToLower(p.CHAddr().String())
	giaddr := p.GIAddr()
	h.info.Lock()
	if !giaddr.Equal(net.IPv4zero) {
		subnet = h.info.FindSubnet(giaddr)
	} else {
		log.Println("Received Broadcast/Local message on ", h.intf.Name)
		addrs, err := h.intf.Addrs()
		if err != nil {
			log.Println("Can't find addresses for ", h.intf.Name, ": ", err)
		}

		for _, a := range addrs {
			aip, _, _ := net.ParseCIDR(a.String())

			// Only operate on v4 addresses
			if aip.To4() == nil {
				continue
			}
			subnet = h.info.FindSubnet(aip)
			if subnet != nil {
				break
			}

		}

		if ignore_anonymus {
			// Search all subnets for a binding. First wins
			log.Println("Looking up bound subnet for ", nic)
			subnet = h.info.FindBoundIP(nic)
		}

		if subnet == nil {
			// We didn't find a subnet for the interface.  Look for the assigned server IP
			subnet = h.info.FindSubnet(h.ip)
		}

	}

	if subnet == nil {
		log.Println("Can not find subnet for packet, ignoring, ", nic)
		h.info.Unlock()
		return
	}

	switch msgType {

	case dhcp.Discover:
		lease, binding := subnet.findOrGetInfo(h.info, nic, p.CIAddr())
		if lease == nil {
			log.Println("Out of IPs for ", subnet.Name, ", ignoring, ", nic)
			h.info.Unlock()
			return nil
		}
		// Ignore unknown MAC address
		if ignore_anonymus && binding == nil {
			h.info.Unlock()
			log.Println("Ignoring request from unknown MAC address, ", nic)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}

		options, lease_time := subnet.buildOptions(lease, binding, p)

		reply := dhcp.ReplyPacket(p, dhcp.Offer,
			h.ip,
			lease.Ip,
			lease_time,
			options.SelectOrderOrAll(options[dhcp.OptionParameterRequestList]))
		h.info.Unlock()
		log.Println("Discover: Handing out: ", reply.YIAddr(), " to ", reply.CHAddr())
		return reply

	case dhcp.Request:
		server, ok := options[dhcp.OptionServerIdentifier]
		if ok && !net.IP(server).Equal(h.ip) {
			h.info.Unlock()
			return nil // Message not for this dhcp server
		}
		reqIP := net.IP(options[dhcp.OptionRequestedIPAddress])
		if reqIP == nil {
			reqIP = net.IP(p.CIAddr())
		}

		if len(reqIP) != 4 || reqIP.Equal(net.IPv4zero) {
			h.info.Unlock()
			log.Println("Request: NAKing: zero req IP ", reqIP, " and ", h.ip)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}

		lease, binding := subnet.findInfo(h.info, nic)
		// Ignore unknown MAC address
		if ignore_anonymus && binding == nil {
			h.info.Unlock()
			log.Println("Ignoring request from unknown MAC address, ", nic)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}
		if lease == nil {
			h.info.Unlock()
			log.Println("Requested IP not found in lease database: ", reqIP, " from ", nic)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}

		if !lease.Ip.Equal(reqIP) {
			h.info.Unlock()
			log.Println("Requested IP doesn't match leased IP: ", reqIP, " ", lease.Ip, " ", nic)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}

		options, lease_time := subnet.buildOptions(lease, binding, p)

		subnet.updateLeaseTime(h.info, lease, lease_time)

		reply := dhcp.ReplyPacket(p, dhcp.ACK,
			h.ip,
			lease.Ip,
			lease_time,
			options.SelectOrderOrAll(options[dhcp.OptionParameterRequestList]))
		if binding != nil && binding.NextServer != nil {
			reply.SetSIAddr(net.ParseIP(*binding.NextServer))
		} else if subnet.NextServer != nil {
			reply.SetSIAddr(*subnet.NextServer)
		}
		h.info.Unlock()
		log.Println("Request: Handing out: ", reply.YIAddr(), " to ", reply.CHAddr())
		return reply

	case dhcp.Release, dhcp.Decline:
		subnet.freeLease(h.info, nic)
		h.info.Unlock()
		reqIP := net.IP(options[dhcp.OptionRequestedIPAddress])
		if reqIP == nil {
			reqIP = net.IP(p.CIAddr())
		}
		log.Println("Release/Decline: from: ", nic, " for ", reqIP)
	}
	return nil
}
