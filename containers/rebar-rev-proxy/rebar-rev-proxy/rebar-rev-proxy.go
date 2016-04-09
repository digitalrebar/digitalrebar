package main

import (
	"crypto/tls"
	"crypto/x509"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"strconv"

	"github.com/kmanley/go-http-auth"
)

var cacert_pem, key_pem, cert_pem string
var auth_filter string
var listen_port int
var db_store_type string
var db_file_name string
var db_consul_key string
var db_initial_json_file string
var digest_realm string

func init() {
	flag.StringVar(&key_pem, "key_pem", "/etc/rev-proxy/server.key", "Path to key file")
	flag.StringVar(&cert_pem, "cert_pem", "/etc/rev-proxy/server.crt", "Path to cert file")
	flag.StringVar(&cacert_pem, "cacert_pem", "/etc/rev-proxy/ca.pem", "Path to cert file")
	flag.IntVar(&listen_port, "listen_port", 8443, "Port to listen on")
	flag.StringVar(&auth_filter, "auth_filter", "none", "Auth Filter to use. Either 'saml', 'basic', 'digest', or 'none'")
	flag.StringVar(&db_store_type, "db_store_type", "consul", "Type of storage area to use: consul or file")
	flag.StringVar(&db_file_name, "db_file_name", "/etc/rev-proxy/db-store.json", "Database storage or initialization file")
	flag.StringVar(&db_consul_key, "db_consul_key", "digitalrebar/private/rebar-rev/db", "Key path for consul db")
	flag.StringVar(&digest_realm, "digest_realm", "digitalrebar", "Default realm for authentication")
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

	if auth_filter == "basic" || auth_filter == "digest" {
		var sp auth.SecretProvider
		if db_store_type == "file" {
			sp, err = JsonFileProvider(db_file_name, auth_filter)
			if err != nil {
				log.Fatal(err)
			}
		} else if db_store_type == "consul" {
			sp, err = JsonConsulProvider(db_consul_key, db_file_name, auth_filter)
			if err != nil {
				log.Fatal(err)
			}
		} else {
			log.Fatal("Unknown db_store_type: %v", db_store_type)
		}

		var authenticator auth.AuthenticatorInterface
		if auth_filter == "basic" {
			authenticator = auth.NewBasicAuthenticator(digest_realm, sp)
		} else {
			authenticator = auth.NewDigestAuthenticator(digest_realm, sp)
		}

		newMux := http.NewServeMux()
		newMux.HandleFunc("/", auth.JustCheck(authenticator, myMux.ServeHTTP))
		handler = newMux
	} else if auth_filter == "saml" {
		handler = NewSamlAuthFilter(myMux, cert_pem, key_pem)
	} else if auth_filter == "none" {
		// Do nothing
	} else {
		log.Fatal("Unknown auth_filter: %v", auth_filter)
	}

	println("starting consul")
	go ServiceRegistry.WatchConsul()

	println("ready")
	log.Fatal(http.ListenAndServeTLS(":"+strconv.Itoa(listen_port), cert_pem, key_pem, handler))
}
