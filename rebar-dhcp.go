package main

import (
	"flag"
	"log"

	"github.com/digitalrebar/gcfg"
)

type Config struct {
	Network struct {
		Port     int
		Username string
		Password string
	}
}

var ignore_anonymus bool
var config_path, key_pem, cert_pem, data_dir string
var backingStore string
var server_ip string

func init() {
	flag.StringVar(&config_path, "config_path", "/etc/rebar-dhcp.conf", "Path to config file")
	flag.StringVar(&key_pem, "key_pem", "/etc/dhcp-https-key.pem", "Path to key file")
	flag.StringVar(&cert_pem, "cert_pem", "/etc/dhcp-https-cert.pem", "Path to cert file")
	flag.StringVar(&data_dir, "data_dir", "/var/cache/rebar-dhcp", "Path to store data.")
	flag.StringVar(&server_ip, "server_ip", "", "Server IP to return in packets (e.g. 10.10.10.1/24)")
	flag.StringVar(&backingStore, "backing_store", "file", "Backing store to use. Either 'consul' or 'file'")
	flag.BoolVar(&ignore_anonymus, "ignore_anonymus", false, "Ignore unknown MAC addresses")
}

func main() {
	flag.Parse()

	var cfg Config
	err := gcfg.ReadFileInto(&cfg, config_path)
	if err != nil {
		log.Fatal(err)
	}
	var bs LoadSaver
	switch backingStore {
	case "file":
		bs, err = NewFileStore(data_dir + "/database.json")
	case "consul":
		bs, err = NewConsulStore(data_dir)
	default:
		log.Fatalf("Unknown backing store type %s", backingStore)
	}

	if err != nil {
		log.Fatal(err)
	}

	fe := NewFrontend(cert_pem, key_pem, cfg, bs)

	if err := StartDhcpHandlers(fe.DhcpInfo, server_ip); err != nil {
		log.Fatal(err)
	}
	fe.RunServer(true)
}
