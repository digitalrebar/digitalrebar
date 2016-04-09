package main

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"

	"flag"
	"strconv"
)

var cacert_pem, key_pem, cert_pem string
var auth_filter string
var listen_port int
var db_store_type string
var db_file_name string
var db_consul_key string
var db_initial_json_file string

func init() {
	flag.StringVar(&key_pem, "key_pem", "/etc/rev-proxy/server.key", "Path to key file")
	flag.StringVar(&cert_pem, "cert_pem", "/etc/rev-proxy/server.crt", "Path to cert file")
	flag.StringVar(&cacert_pem, "cacert_pem", "/etc/rev-proxy/ca.pem", "Path to cert file")
	flag.IntVar(&listen_port, "listen_port", 8443, "Port to listen on")
	flag.StringVar(&auth_filter, "auth_filter", "none", "Auth Filter to use. Either 'saml', 'none', or 'db'")
	flag.StringVar(&db_file_name, "db_file_name", "/var/cache/rev_db.json", "Database storage file")
	flag.StringVar(&db_store_type, "db_store_type", "consul", "Type of storage area to use: consul or file")
	flag.StringVar(&db_consul_key, "db_consul_key", "digitalrebar/private/rebar-rev/db", "Key path for consul db")
	flag.StringVar(&db_initial_json_file, "db_initial_json_file", "/etc/rev-proxy/db-init.json", "Initial json db file")
}

func main() {
	flag.Parse()

	// Load client cert
	cert, err := tls.LoadX509KeyPair(cert_pem, key_pem)
	if err != nil {
		log.Fatal(err)
	}

	// Load CA cert
	caCert, err := ioutil.ReadFile(cacert_pem)
	if err != nil {
		log.Fatal(err)
	}

	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM(caCert)
	// Setup HTTPS client
	tlsConfig := &tls.Config{
		Certificates: []tls.Certificate{cert},
		RootCAs:      caCertPool,
		ClientAuth:   tls.RequireAndVerifyClientCert,
	}
	tlsConfig.BuildNameToCertificate()

	// Service multiplexer
	var handler http.Handler
	myMux := http.NewServeMux()
	myMux.HandleFunc("/", NewMultipleHostReverseProxy(ServiceRegistry, tlsConfig))
	myMux.HandleFunc("/health", func(w http.ResponseWriter, req *http.Request) {
		fmt.Fprintf(w, "%v\n", ServiceRegistry)
	})
	handler = myMux

	if auth_filter != "none" {
		if auth_filter == "saml" {
			handler = NewSamlAuthFilter(myMux, cert_pem, key_pem)
		} else if auth_filter == "db" {
			var store LoadSaver
			if db_store_type == "consul" {
				store, err = NewFileStore(db_file_name, db_initial_json_file)
				if err != nil {
					log.Fatal(err)
				}
			} else if db_store_type == "file" {
				store, err = NewConsulStore(db_consul_key, db_initial_json_file)
				if err != nil {
					log.Fatal(err)
				}
			} else {
				log.Fatal("Unknown db_store_type: %v", db_store_type)
			}

			handler = NewDBAuthFilter(myMux, store)
		} else {
			log.Fatal("Unknown auth_filter: %v", auth_filter)
		}
	}

	println("starting consul")
	go ServiceRegistry.WatchConsul()

	println("ready")
	log.Fatal(http.ListenAndServeTLS(":"+strconv.Itoa(listen_port), cert_pem, key_pem, handler))
}
