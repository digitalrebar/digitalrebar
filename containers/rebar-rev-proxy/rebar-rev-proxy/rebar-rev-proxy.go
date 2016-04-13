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
var saml_idpssourl string
var saml_idpssodescurl string
var saml_idpcert string

func init() {
	flag.StringVar(&key_pem, "key_pem", "/etc/rev-proxy/server.key", "Path to key file")
	flag.StringVar(&cert_pem, "cert_pem", "/etc/rev-proxy/server.crt", "Path to cert file")
	flag.StringVar(&cacert_pem, "cacert_pem", "/etc/rev-proxy/ca.pem", "Path to cert file")
	flag.IntVar(&listen_port, "listen_port", 8443, "Port to listen on")
	flag.StringVar(&auth_filter, "auth_filter", "digest", "Auth Filter to use. Either 'saml', 'basic', 'digest', or 'none'")
	flag.StringVar(&db_store_type, "db_store_type", "file", "Type of storage area to use: consul or file")
	flag.StringVar(&db_file_name, "db_file_name", "/etc/rev-proxy/db-store.json", "Database storage or initialization file")
	flag.StringVar(&db_consul_key, "db_consul_key", "digitalrebar/private/rebar-rev/db", "Key path for consul db")
	flag.StringVar(&digest_realm, "digest_realm", "Rebar", "Default realm for authentication")

	flag.StringVar(&saml_idpssourl, "saml_idpssourl", "https://dev-888522.oktapreview.com/app/rackndev888522_rackn_1/exk5ui8zc112R5ioP0h7/sso/saml", "Default Identity Provider SSO URL")
	flag.StringVar(&saml_idpssodescurl, "saml_idpssodescurl", "http://www.okta.com/exk5ui8zc112R5ioP0h7", "Default Identity Provider SSO Descriptor URL")
	flag.StringVar(&saml_idpcert, "saml_idpcert", "/etc/rev-proxy/saml-dir/idpcert.crt", "Default SAML SSO Cert")
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
	var auth_handler http.Handler
	var base_handler http.Handler
	myMux := http.NewServeMux()
	myMux.HandleFunc("/", NewMultipleHostReverseProxy(ServiceRegistry, tlsConfig))
	myMux.HandleFunc("/health", func(w http.ResponseWriter, req *http.Request) {
		fmt.Fprintf(w, "%v\n", ServiceRegistry)
	})
	base_handler = myMux

	if auth_filter == "basic" || auth_filter == "digest" {
		var sp auth.SecretProvider
		var cp CapabilityProvider
		if db_store_type == "file" {
			sp, cp, err = JsonFileProvider(db_file_name, auth_filter)
			if err != nil {
				log.Fatal(err)
			}
		} else if db_store_type == "consul" {
			sp, cp, err = JsonConsulProvider(db_consul_key, db_file_name, auth_filter)
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
		// Override the builtin wrap function to include our capabilities.
		newMux.HandleFunc("/", authenticator.Wrap(func(w http.ResponseWriter, ar *auth.AuthenticatedRequest) {
			cap := cp(ar.Username, digest_realm)
			t := register_user(ar.Username, digest_realm, cap)
			add_token_info(t, w)

			myMux.ServeHTTP(w, &ar.Request)
		}))
		auth_handler = newMux
	} else if auth_filter == "saml" {
		auth_handler = NewSamlAuthFilter(myMux, cert_pem, key_pem, saml_idpssourl, saml_idpssodescurl, saml_idpcert)
	} else if auth_filter == "none" {
		// Do nothing
	} else {
		log.Fatal("Unknown auth_filter: %v", auth_filter)
	}

	tokenMux := http.NewServeMux()
	tokenMux.HandleFunc("/api/license", NewMultipleHostReverseProxy(ServiceRegistry, tlsConfig))
	tokenMux.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		token := has_token_info(req)
		if token == nil && auth_handler != nil {
			auth_handler.ServeHTTP(w, req)
		} else {
			base_handler.ServeHTTP(w, req)
		}
	})

	println("starting consul")
	go ServiceRegistry.WatchConsul()

	println("ready")
	log.Fatal(http.ListenAndServeTLS(":"+strconv.Itoa(listen_port), cert_pem, key_pem, tokenMux))
}
