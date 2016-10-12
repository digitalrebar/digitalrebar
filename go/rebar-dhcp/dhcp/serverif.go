package dhcp

import (
	"net"
	"strconv"

	"golang.org/x/net/ipv4"
)

type serveIfConn struct {
	handlers map[int]Handler
	conn     *ipv4.PacketConn
	cm       *ipv4.ControlMessage
}

func (s *serveIfConn) ReadFrom(b []byte) (n int, addr net.Addr, handler Handler, err error) {
	n, s.cm, addr, err = s.conn.ReadFrom(b)
	if s.cm != nil {
		var ok bool
		handler, ok = s.handlers[s.cm.IfIndex]
		if !ok {
			n = 0 // Packets < 240 are filtered in Serve().
		}
	}
	return
}

func (s *serveIfConn) WriteTo(b []byte, addr net.Addr) (n int, err error) {

	// ipv4 docs state that Src is "specify only", however testing by tfheen
	// shows that Src IS populated.  Therefore, to reuse the control message,
	// we set Src to nil to avoid the error "write udp4: invalid argument"
	s.cm.Src = nil

	return s.conn.WriteTo(b, s.cm, addr)
}

// Serve takes a ServeConn (such as a net.PacketConn) that it uses for both
// reading and writing DHCP packets. Every packet is passed to the handler,
// which processes it and optionally return a response packet for writing back
// to the network.
//
// To capture limited broadcast packets (sent to 255.255.255.255), you must
// listen on a socket bound to IP_ADDRANY (0.0.0.0). This means that broadcast
// packets sent to any interface on the system may be delivered to this
// socket.  See: https://code.google.com/p/go/issues/detail?id=7106
//
// Additionally, response packets may not return to the same
// interface that the request was received from.  Writing a custom ServeConn,
// or using ServeIf() can provide a workaround to this problem.
func ServeIf2(conn *serveIfConn) error {
	buffer := make([]byte, 1500)
	for {
		n, addr, handler, err := conn.ReadFrom(buffer)
		if err != nil {
			return err
		}
		if n < 240 { // Packet too small to be DHCP
			continue
		}
		req := Packet(buffer[:n])
		if req.HLen() > 16 { // Invalid size
			continue
		}
		options := req.ParseOptions()
		var reqType MessageType
		if t := options[OptionDHCPMessageType]; len(t) != 1 {
			continue
		} else {
			reqType = MessageType(t[0])
			if reqType < Discover || reqType > Inform {
				continue
			}
		}
		if res := handler.ServeDHCP(req, reqType, options); res != nil {
			// If IP not available, broadcast
			ipStr, portStr, err := net.SplitHostPort(addr.String())
			if err != nil {
				return err
			}

			if net.ParseIP(ipStr).Equal(net.IPv4zero) || req.Broadcast() {
				port, _ := strconv.Atoi(portStr)
				addr = &net.UDPAddr{IP: net.IPv4bcast, Port: port}
			}
			if _, e := conn.WriteTo(res, addr); e != nil {
				return e
			}
		}
	}
}

// ServeIf does the same job as Serve(), but listens and responds on the
// specified network interface (by index).  It also doubles as an example of
// how to leverage the dhcp4.ServeConn interface.
//
// If your target only has one interface, use Serve(). ServeIf() requires an
// import outside the std library.  Serving DHCP over multiple interfaces will
// require your own dhcp4.ServeConn, as listening to broadcasts utilises all
// interfaces (so you cannot have more than on listener).
func ServeIf(conn net.PacketConn, handlers map[int]Handler) error {
	p := ipv4.NewPacketConn(conn)
	if err := p.SetControlMessage(ipv4.FlagInterface, true); err != nil {
		return err
	}
	return ServeIf2(&serveIfConn{handlers: handlers, conn: p})
}

// ListenAndServe listens on the UDP network address addr and then calls
// Serve with handler to handle requests on incoming packets.
// i.e. ListenAndServeIf("eth0",handler)
func ListenAndServeIf(handlers map[int]Handler) error {
	l, err := net.ListenPacket("udp4", ":67")
	if err != nil {
		return err
	}
	defer l.Close()
	return ServeIf(l, handlers)
}
