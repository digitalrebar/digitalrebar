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

func main() {

	var cfg Config
	cerr := gcfg.ReadFileInto(&cfg, "config.gcfg")
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
	log.Fatal(http.ListenAndServeTLS(connStr, "https-cert.pem", "https-key.pem", api.MakeHandler()))
}
