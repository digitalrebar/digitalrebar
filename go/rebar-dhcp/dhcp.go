// Example of minimal DHCP server:
package main

import (
	"encoding/binary"
	"fmt"
	"log"
	"net"
	"strings"
	"time"

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

func xid(p dhcp.Packet) string {
	return fmt.Sprintf("xid 0x%x", binary.BigEndian.Uint32(p.XId()))
}

type DHCPHandler struct {
	intf net.Interface // Interface processing on.
	ip   net.IP        // Server IP to use
	info *DataTracker  // Subnet data
}

func findSubnet(h *DHCPHandler, p dhcp.Packet) *Subnet {
	if !p.GIAddr().Equal(net.IPv4zero) {
		log.Printf("%s: relay from %s (hops %d)", xid(p), p.GIAddr(), p.Hops())
		return h.info.FindSubnet(p.GIAddr())
	}
	log.Printf("%s: local from %s", xid(p), h.intf.Name)
	addrs, err := h.intf.Addrs()
	if err != nil {
		log.Printf("%s: Can't find addresses for %s: %v", xid(p), h.intf.Name, err)
		return nil
	}
	for _, a := range addrs {
		aip, _, _ := net.ParseCIDR(a.String())

		// Only operate on v4 addresses
		if aip.To4() == nil {
			continue
		}
		if subnet := h.info.FindSubnet(aip); subnet != nil {
			return subnet
		}

	}
	if ignoreAnonymus {
		// Search all subnets for a binding. First wins
		nic := strings.ToLower(p.CHAddr().String())
		log.Printf("%s: Looking up bound subnet for %s", xid(p), nic)
		if subnet := h.info.FindBoundIP(nic); subnet != nil {
			return subnet
		}
	}
	// We didn't find a subnet for the interface.  Look for the assigned server IP
	return h.info.FindSubnet(h.ip)
}

func (h *DHCPHandler) ServeDHCP(p dhcp.Packet, msgType dhcp.MessageType, options dhcp.Options) (d dhcp.Packet) {

	log.Printf("Recieved DHCP packet: type %s %s ciaddr %s yiaddr %s giaddr %s chaddr %s",
		msgType.String(),
		xid(p),
		p.CIAddr(),
		p.YIAddr(),
		p.GIAddr(),
		p.CHAddr().String())
	log.Printf("%s: Starting processing: %s", xid(p), time.Now())
	h.info.Lock()
	defer h.info.Unlock()
	log.Printf("%s: Config lock acquired: %s", xid(p), time.Now())
	subnet := findSubnet(h, p)
	if subnet == nil {
		log.Printf("%s %s: No subnet for leases", msgType.String(), xid(p))
		return nil
	}
	log.Printf("%s %s: found subnet %v", msgType.String(), xid(p), subnet.Subnet)
	nic := strings.ToLower(p.CHAddr().String())
	switch msgType {

	case dhcp.Discover:
		lease, binding := subnet.findOrGetInfo(h.info, nic, p.CIAddr())
		if lease == nil {
			log.Printf("%s: Discovery out of IPs for %s, ignoring %v", xid(p), subnet.Name, nic)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}
		if ignoreAnonymus && binding == nil {
			log.Printf("%s: Discovery ignoring request from unknown MAC address %s: %v",
				xid(p),
				p.CHAddr().String(),
				nic)
			return nil
		}

		options, leaseTime := subnet.buildOptions(lease, binding, p)
		reply := dhcp.ReplyPacket(p, dhcp.Offer,
			h.ip,
			lease.Ip,
			leaseTime,
			options.SelectOrderOrAll(options[dhcp.OptionParameterRequestList]))

		log.Printf("%s: Discovery handing out: %s to %s", xid(p),
			reply.YIAddr(),
			reply.CHAddr())
		return reply

	case dhcp.Request:
		server, ok := options[dhcp.OptionServerIdentifier]
		if ok && !net.IP(server).Equal(h.ip) {
			log.Printf("%s: Request message for DHCP server %s, not us. Ignoring",
				xid(p),
				server)
			return nil // Message not for this dhcp server
		}
		reqIP := net.IP(options[dhcp.OptionRequestedIPAddress])
		if reqIP == nil {
			reqIP = net.IP(p.CIAddr())
		}

		if len(reqIP) != 4 || reqIP.Equal(net.IPv4zero) {
			log.Printf("%s: Request NAKing: zero req IP %s and %s",
				xid(p),
				reqIP,
				h.ip)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}

		lease, binding := subnet.findInfo(h.info, nic)
		// Ignore unknown MAC address
		if ignoreAnonymus && binding == nil {
			log.Printf("%s: Request ignoring request from unknown MAC address %s",
				xid(p),
				nic)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}
		if lease == nil {
			log.Printf("%s Request IP %s from %s not found in lease database",
				xid(p),
				reqIP,
				nic)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}

		if !lease.Ip.Equal(reqIP) {
			log.Printf("%s: Request IP %s from %s doesn't match leased IP %s",
				xid(p),
				reqIP,
				nic,
				lease.Ip)
			return dhcp.ReplyPacket(p, dhcp.NAK, h.ip, nil, 0, nil)
		}

		options, leaseTime := subnet.buildOptions(lease, binding, p)

		subnet.updateLeaseTime(h.info, lease, leaseTime)

		reply := dhcp.ReplyPacket(p, dhcp.ACK,
			h.ip,
			lease.Ip,
			leaseTime,
			options.SelectOrderOrAll(options[dhcp.OptionParameterRequestList]))
		if binding != nil && binding.NextServer != nil {
			reply.SetSIAddr(net.ParseIP(*binding.NextServer))
		} else if subnet.NextServer != nil {
			reply.SetSIAddr(*subnet.NextServer)
		}
		log.Printf("%s: Request handing out %s to %s",
			xid(p),
			reply.YIAddr(),
			reply.CHAddr())
		return reply

	case dhcp.Release, dhcp.Decline:
		subnet.freeLease(h.info, nic)
		reqIP := net.IP(options[dhcp.OptionRequestedIPAddress])
		if reqIP == nil {
			reqIP = net.IP(p.CIAddr())
		}
		log.Printf("%s: Release/Decline from %s for %s",
			xid(p),
			nic,
			reqIP)
	}
	return nil
}
