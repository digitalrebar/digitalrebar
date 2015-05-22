package main

/* These are needed for database access
 *   "database/sql"
 *    _ "github.com/lib/pq"
 *
 * These are needed for consul api accces
 *    "github.com/hashicorp/consul/api"
 */

import (
	"code.google.com/p/gcfg"
	"flag"
	"fmt"
	"github.com/ant0ine/go-json-rest/rest"
	"log"
	"net/http"
)

type Config struct {
	PowerDNS struct {
		Password string
		Hostname string
		Port     int
		Server   string
	}
	Network struct {
		Port     int
		Username string
		Password string
	}
}

type DnsInstance struct {
	UrlBase     string
	AccessToken string
}

var config_path, key_pem, cert_pem string

func init() {
	flag.StringVar(&config_path, "config_path", "/etc/dns-mgmt.conf", "Path to config file")
	flag.StringVar(&key_pem, "key_pem", "/etc/dns-mgmt-https-key.pem", "Path to config file")
	flag.StringVar(&cert_pem, "cert_pem", "/etc/dns-mgmt-https-cert.pem", "Path to config file")
}

func main() {
	flag.Parse()

	var cfg Config
	cerr := gcfg.ReadFileInto(&cfg, config_path)
	if cerr != nil {
		log.Fatal(cerr)
	}

	base := fmt.Sprintf("http://%s:%d/servers/%s", cfg.PowerDNS.Hostname, cfg.PowerDNS.Port, cfg.PowerDNS.Server)
	di := DnsInstance{
		UrlBase:     base,
		AccessToken: cfg.PowerDNS.Password,
	}

	api := rest.NewApi()
	api.Use(&rest.AuthBasicMiddleware{
		Realm: "test zone",
		Authenticator: func(userId string, password string) bool {
			if userId == cfg.Network.Username && password == cfg.Network.Password {
				return true
			}
			return false
		},
	})
	api.Use(rest.DefaultDevStack...)
	router, err := rest.MakeRouter(
		rest.Get("/zones", di.GetAllZones),
		rest.Post("/zones", di.PostZone),
		rest.Get("/zones/#id", di.GetZone),
		rest.Put("/zones/#id", di.PutZone),
		rest.Delete("/zones/#id", di.DeleteZone),
		&rest.Route{"PATCH", "/zones/#id", di.PatchZone},
	)
	if err != nil {
		log.Fatal(err)
	}
	api.SetApp(router)

	connStr := fmt.Sprintf(":%d", cfg.Network.Port)
	log.Println("Using", connStr)
	log.Fatal(http.ListenAndServeTLS(connStr, cert_pem, key_pem, api.MakeHandler()))
}
