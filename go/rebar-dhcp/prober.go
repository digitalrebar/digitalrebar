package main

import (
	"bytes"
	"fmt"
	"log"
	"net"
	"sync"
	"time"

	"golang.org/x/net/icmp"
	"golang.org/x/net/ipv4"
)

type Prober struct {
	sync.Mutex
	probes   map[string][]chan<- bool
	timeouts map[string]time.Time
	conn     *icmp.PacketConn
}

func (p *Prober) msgBody(addr string) []byte {
	return []byte(fmt.Sprintf("Rebar DHCP Address Probe %s", addr))
}

func (p *Prober) InUse(addr net.IP, timeout time.Duration) <-chan bool {
	p.Lock()
	defer p.Unlock()
	ip := addr.String()
	res := make(chan bool)
	if probes, ok := p.probes[ip]; ok {
		probes = append(probes, res)
	} else {
		p.probes[ip] = []chan<- bool{res}
		go func() {
			for i := 1; i <= 3; i++ {
				tgtAddr := &net.IPAddr{IP: addr}
				msgBody := p.msgBody(tgtAddr.IP.String())
				msg := icmp.Message{
					Type: ipv4.ICMPTypeEcho,
					Code: 0,
					Body: &icmp.Echo{
						Data: msgBody,
						Seq:  i,
					},
				}
				msgBytes, err := msg.Marshal(nil)
				if err == nil {
					_, err = p.conn.WriteTo(msgBytes, tgtAddr)
				}
				time.Sleep(1 * time.Second)
			}
		}()
	}
	p.timeouts[ip] = time.Now().Add(timeout)
	return res
}

func (p *Prober) runTimeouts() ([]chan<- bool, bool) {
	p.Lock()
	defer p.Unlock()
	cTime := time.Now()
	toKill := []string{}
	for k, v := range p.timeouts {
		if cTime.After(v) {
			toKill = append(toKill, k)
		}
	}
	res := []chan<- bool{}
	if len(toKill) > 0 {
		for _, v := range toKill {
			delete(p.timeouts, v)
			res = append(res, p.probes[v]...)
			delete(p.probes, v)
		}
	}
	return res, false
}

func (p *Prober) runMessage(peer net.Addr, pktLen int, buf []byte) ([]chan<- bool, bool) {
	res := []chan<- bool{}
	retVal := false
	toKill := ""
	var resp *icmp.Message
	resp, err := icmp.ParseMessage(1, buf[:pktLen])
	if err != nil {
		return res, false
	}
	// No read error, so see what kind of ICMP packet we recieved.
	tgtAddr := peer.String()
	p.Lock()
	defer p.Unlock()
	switch resp.Type {
	case ipv4.ICMPTypeDestinationUnreachable:
		// DestinationUnreachable will not come from our target, so we
		// have to test its body against all our potential targets.
		body, ok := resp.Body.(*icmp.DstUnreach)
		if ok {
			for k := range p.probes {
				msgBody := p.msgBody(k)
				if bytes.Contains(body.Data, msgBody) {
					res = p.probes[k]
					toKill = k
					break
				}
			}
		}
	case ipv4.ICMPTypeEchoReply:
		_, ok := p.probes[tgtAddr]
		if ok {
			res = p.probes[tgtAddr]
			toKill = tgtAddr
			retVal = true
		}
	}
	if toKill != "" {
		delete(p.timeouts, toKill)
		delete(p.probes, toKill)
	}
	return res, retVal
}

func (p *Prober) mainLoop() {
	buf := make([]byte, 1500)
	for {
		chansToSend := []chan<- bool{}
		valToSend := false
		err := p.conn.SetReadDeadline(time.Now().Add(1 * time.Second))
		if err != nil {
			p.conn.Close()
			log.Panicf("Could not set read deadline on ICMP read request: %v", err)
		}
		n, peer, err := p.conn.ReadFrom(buf)
		if err != nil {
			chansToSend, valToSend = p.runTimeouts()
		} else {
			chansToSend, valToSend = p.runMessage(peer, n, buf)
		}
		for _, ch := range chansToSend {
			ch <- valToSend
			close(ch)
		}
	}
}

func NewProber() (*Prober, error) {
	res := &Prober{
		probes:   map[string][]chan<- bool{},
		timeouts: map[string]time.Time{},
	}
	conn, err := icmp.ListenPacket("ip4:icmp", "0.0.0.0")
	if err != nil {
		return nil, err
	}
	res.conn = conn
	go res.mainLoop()
	return res, nil
}
