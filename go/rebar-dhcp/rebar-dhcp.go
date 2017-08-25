package main

import (
	"flag"
	"log"

	"github.com/digitalrebar/digitalrebar/go/common/store"
	"github.com/digitalrebar/digitalrebar/go/common/version"
	rebar "github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	consul "github.com/hashicorp/consul/api"
)

var ignoreAnonymus bool
var dataDir string
var backingStore string
var serverIp string
var hostString string
var serverPort int
var versionFlag bool
var rebarClient *rebar.Client
var prober *Prober

func init() {
	flag.BoolVar(&versionFlag, "version", false, "Print version and exit")
	flag.StringVar(&dataDir, "dataDir", "/var/cache/rebar-dhcp", "Path to store data.")
	flag.StringVar(&serverIp, "serverIp", "", "Server IP to return in packets (e.g. 10.10.10.1/24)")
	flag.StringVar(&backingStore, "backingStore", "file", "Backing store to use. Either 'consul' or 'file'")
	flag.BoolVar(&ignoreAnonymus, "ignoreAnonymus", false, "Ignore unknown MAC addresses")
	flag.StringVar(&hostString, "host", "dhcp,dhcp-mgmt,localhost,127.0.0.1", "Comma separated list of hosts to put in certificate")
	flag.IntVar(&serverPort, "port", 6755, "Management access port")
}

func main() {
	flag.Parse()

	if versionFlag {
		log.Fatalf("Version: %s", version.REBAR_VERSION)
	}
	log.Printf("Version: %s\n", version.REBAR_VERSION)

	var bs store.SimpleStore
	switch backingStore {
	case "file":
		var err error
		bs, err = store.NewSimpleLocalStore(dataDir)
		if err != nil {
			log.Fatal(err)
		}
	case "consul":
		c, err := consul.NewClient(consul.DefaultConfig())
		if err == nil {
			bs, err = store.NewSimpleConsulStore(c, dataDir)
		}
		if err != nil {
			log.Fatal(err)
		}
	default:
		log.Fatalf("Unknown backing store type %s", backingStore)
	}
	var err error
	rebarClient, err = rebar.TrustedSession("system", true)
	if err != nil {
		log.Fatal(err)
	}
	fe := NewFrontend(bs)
	prober, err = NewProber()
	if err != nil {
		log.Fatal(err)
	}
	if err := StartDhcpHandlers(fe.DhcpInfo, serverIp); err != nil {
		log.Fatal(err)
	}
	fe.RunServer(true)
}
