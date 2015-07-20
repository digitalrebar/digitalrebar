package main

import (
	"code.google.com/p/gcfg"
	"flag"
	"log"
	"net"
)

type Config struct {
	Network struct {
		Port     int
		Username string
		Password string
	}
}

var config_path, key_pem, cert_pem, data_dir string

func init() {
	flag.StringVar(&config_path, "config_path", "/etc/ocb-dhcp.conf", "Path to config file")
	flag.StringVar(&key_pem, "key_pem", "/etc/dhcp-https-key.pem", "Path to key file")
	flag.StringVar(&cert_pem, "cert_pem", "/etc/dhcp-https-cert.pem", "Path to cert file")
	flag.StringVar(&data_dir, "data_dir", "/var/cache/ocb-dhcp", "Path to store data")
}

func main() {
	flag.Parse()

	var cfg Config
	cerr := gcfg.ReadFileInto(&cfg, config_path)
	if cerr != nil {
		log.Fatal(cerr)
	}

	fe := NewFrontend(data_dir, cert_pem, key_pem, cfg)

	intfs, err := net.Interfaces()
	if err != nil {
		log.Fatal(err)
	}
	var serverIp *net.Addr
	serverIp = nil
	for _, intf := range intfs {
		if (intf.Flags & net.FlagLoopback) == net.FlagLoopback {
			continue
		}
		if (intf.Flags & net.FlagUp) != net.FlagUp {
			continue
		}
		if serverIp == nil {
			addrs, err := intf.Addrs()
			if err != nil {
				log.Fatal(err)
			}
			serverIp = &addrs[0]
		}
		go RunDhcpHandler(fe.DhcpInfo, intf, serverIp)
	}

	fe.RunServer()
}
