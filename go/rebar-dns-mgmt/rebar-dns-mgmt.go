package main

/* These are needed for database access
 *   "database/sql"
 *    _ "github.com/lib/pq"
 *
 * These are needed for consul api accces
 *    "github.com/hashicorp/consul/api"
 */

import (
	"flag"
	"fmt"
	"log"
	"strings"

	"github.com/ant0ine/go-json-rest/rest"
	"github.com/digitalrebar/go-common/cert"
	"github.com/digitalrebar/go-common/version"
)

var dataDir, backingStore, hostString string
var dnsType, dnsServer, dnsHostname, dnsPassword string
var serverPort, dnsPort int
var versionFlag bool

func init() {
	flag.BoolVar(&versionFlag, "version", false, "Print version and exit")
	flag.StringVar(&dataDir, "dataDir", "/var/cache/rebar-dns-mgmt", "Path to store data")
	flag.StringVar(&backingStore, "backingStore", "file", "Backing store to use. Either 'consul' or 'file'")
	flag.StringVar(&hostString, "host", "dns-mgmt,127.0.0.1,localhost", "Comma separated list of hosts to put in certificate")
	flag.IntVar(&serverPort, "serverPort", 6754, "Server Port")
	// For NSUPDATE, Dns.Server is ip of server to update
	// For PDNS, Dns.Server to access (localhost)
	// For BIND, Dns.Server name (FQDN of DNS server)
	flag.StringVar(&dnsType, "dnsType", "BIND", "Type of DNS server to manage: BIND, PDNS, NSUPDATE")
	flag.StringVar(&dnsServer, "dnsServer", "", "DNS Server access ip or address")
	flag.StringVar(&dnsHostname, "dnsHostname", "", "DNS Hostname for defining server")
	flag.IntVar(&dnsPort, "dnsPort", 6754, "DNS Port for accessing remote server")
	flag.StringVar(&dnsPassword, "dnsPassword", "", "DNS Password for accessing remote server")
}

func main() {
	flag.Parse()

	if versionFlag {
		log.Fatalf("Version: %s", version.REBAR_VERSION)
	}

	log.Printf("Version: %s\n", version.REBAR_VERSION)

	var be dns_backend_point
	var err error

	if dnsType == "BIND" {
		be = NewBindDnsInstance(dnsServer)
	} else if dnsType == "POWERDNS" {
		base := fmt.Sprintf("http://%s:%d/servers/%s", dnsHostname, dnsPort, dnsServer)
		be = &PowerDnsInstance{
			UrlBase:     base,
			AccessToken: dnsPassword,
		}
	} else if dnsType == "NSUPDATE" {
		be = NewNsupdateDnsInstance(dnsServer)
	} else {
		log.Fatal("Failed to find type")
	}
	var bs LoadSaver
	switch backingStore {
	case "file":
		bs, err = NewFileStore(dataDir + "/database.json")
	case "consul":
		bs, err = NewConsulStore(dataDir)
	default:
		log.Fatalf("Unknown backing store type %s", backingStore)
	}

	fe := NewFrontend(&be, bs)

	fe.load_data()

	api := rest.NewApi()
	api.Use(rest.DefaultDevStack...)
	router, err := rest.MakeRouter(
		rest.Get("/zones", fe.GetAllZones),
		rest.Get("/zones/#id", fe.GetZone),
		&rest.Route{"PATCH", "/zones/#id", fe.PatchZone},
	)
	if err != nil {
		log.Fatal(err)
	}
	api.SetApp(router)

	connStr := fmt.Sprintf(":%d", serverPort)
	log.Println("Using", connStr)
	acceptingRoot := "internal"
	hosts := strings.Split(hostString, ",")
	log.Fatal(cert.StartTLSServer(connStr, "dns-mgmt", hosts, acceptingRoot, "internal", api.MakeHandler()))
}
