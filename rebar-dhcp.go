package main

import (
	"flag"
	"log"

	"github.com/digitalrebar/gcfg"
	"github.com/digitalrebar/go-common/store"
	consul "github.com/hashicorp/consul/api"
)

type Config struct {
	Network struct {
		Port     int
		Username string
		Password string
	}
}

var ignore_anonymus bool
var auth_mode, config_path, data_dir string
var backingStore string
var server_ip string

func init() {
	flag.StringVar(&config_path, "config_path", "/etc/rebar-dhcp.conf", "Path to config file")
	flag.StringVar(&data_dir, "data_dir", "/var/cache/rebar-dhcp", "Path to store data.")
	flag.StringVar(&server_ip, "server_ip", "", "Server IP to return in packets (e.g. 10.10.10.1/24)")
	flag.StringVar(&backingStore, "backing_store", "file", "Backing store to use. Either 'consul' or 'file'")
	flag.StringVar(&auth_mode, "auth_mode", "BASIC", "Choose auth method: BASIC, KEY")
	flag.BoolVar(&ignore_anonymus, "ignore_anonymus", false, "Ignore unknown MAC addresses")
}

func main() {
	flag.Parse()

	var cfg Config
	err := gcfg.ReadFileInto(&cfg, config_path)
	if err != nil {
		log.Fatal(err)
	}
	var bs store.SimpleStore
	switch backingStore {
	case "file":
		bs, err = store.NewSimpleLocalStore(data_dir)
	case "consul":
		var c *consul.Client
		c, err = consul.NewClient(consul.DefaultConfig())
		if err == nil {
			bs, err = store.NewSimpleConsulStore(c, data_dir)
		}
	default:
		log.Fatalf("Unknown backing store type %s", backingStore)
	}

	if err != nil {
		log.Fatal(err)
	}

	fe := NewFrontend(cfg, bs)

	if err := StartDhcpHandlers(fe.DhcpInfo, server_ip); err != nil {
		log.Fatal(err)
	}
	fe.RunServer(true, auth_mode)
}
