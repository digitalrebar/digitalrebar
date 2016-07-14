package main

import (
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"strconv"

	"github.com/dgrijalva/jwt-go"
	"github.com/hashicorp/consul/api"
)

var cacertPem, keyPem, certPem string
var authFilter string
var listenPort int
var dbStoreType string
var dbFilename string
var dbConsulKey string
var dbInitialJsonFile string
var digestRealm string
var samlIdpssourl string
var samlIdpssodescurl string
var samlIdpcert string
var externalIp string
var forwarderMode bool

func init() {
	flag.StringVar(&keyPem, "keyPem", "/etc/rev-proxy/server.key", "Path to key file")
	flag.StringVar(&certPem, "certPem", "/etc/rev-proxy/server.crt", "Path to cert file")
	flag.StringVar(&cacertPem, "cacertPem", "/etc/rev-proxy/ca.pem", "Path to cert file")
	flag.IntVar(&listenPort, "listenPort", 443, "Port to listen on")
	flag.StringVar(&authFilter, "authFilter", "digest", "Auth Filter to use. Either 'saml', 'digest', or 'none'")
	flag.StringVar(&digestRealm, "digestRealm", "Rebar", "Default realm for authentication")

	flag.StringVar(&samlIdpssourl, "samlIdpssourl", "https://dev-888522.oktapreview.com/app/rackndev888522_rackn_1/exk5ui8zc112R5ioP0h7/sso/saml", "Default Identity Provider SSO URL")
	flag.StringVar(&samlIdpssodescurl, "samlIdpssodescurl", "http://www.okta.com/exk5ui8zc112R5ioP0h7", "Default Identity Provider SSO Descriptor URL")
	flag.StringVar(&samlIdpcert, "samlIdpcert", "/etc/rev-proxy/saml-dir/idpcert.crt", "Default SAML SSO Cert")
	flag.StringVar(&externalIp, "externalIp", "127.0.0.1", "Server IP to advertise for SAML")
	flag.BoolVar(&forwarderMode, "forwarder", false, "Add if the container is in forwarder mode")
}

func addCorsHeader(w http.ResponseWriter, req *http.Request) {
	origin := req.Header.Get("Origin")
	if origin != "" {
		w.Header().Set("Access-Control-Allow-Origin", origin)
		w.Header().Set("Access-Control-Allow-Headers", "DR-AUTH-TOKEN,X-Requested-With,Content-Type,Cookie,Authorization,WWW-Authenticate") // If-Modified-Since,If-None-Match,
		w.Header().Set("Access-Control-Allow-Headers", "DR-USER-TOKEN,X-Requested-With,Content-Type,Cookie,Authorization,WWW-Authenticate") // If-Modified-Since,If-None-Match,
		w.Header().Set("Access-Control-Allow-Credentials", "true")
		w.Header().Set("Access-Control-Expose-Headers", "DR-AUTH-TOKEN,WWW-Authenticate, Set-Cookie, Access-Control-Allow-Headers, Access-Control-Allow-Credentials, Access-Control-Allow-Origin")
		w.Header().Set("Access-Control-Expose-Headers", "DR-USER-TOKEN,WWW-Authenticate, Set-Cookie, Access-Control-Allow-Headers, Access-Control-Allow-Credentials, Access-Control-Allow-Origin")
	}
}

func main() {
	flag.Parse()

	// Load client cert
	cert, err := tls.LoadX509KeyPair(certPem, keyPem)
	if err != nil {
		log.Printf("Failed to read client certs\n")
		log.Fatal(err)
	}

	// Load CA cert
	caCert, err := ioutil.ReadFile(cacertPem)
	if err != nil {
		log.Printf("Failed to read cacert\n")
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

	randString, err := getTokenSecretKey()
	if err != nil {
		log.Printf("Failed to find/generate secret key\n")
		log.Fatal(err)
	}
	tokenManager := NewJwtManager([]byte(randString),
		JwtConfig{Method: jwt.SigningMethodHS256, TTL: 3600 * 24 * 3})

	// Service multiplexer
	serviceMux := http.NewServeMux()
	serviceMux.HandleFunc("/", NewMultipleHostReverseProxy(ServiceRegistry, tlsConfig))
	serviceMux.HandleFunc("/health", func(w http.ResponseWriter, req *http.Request) {
		b, err := json.Marshal(ServiceRegistry)
		if err != nil {
			fmt.Fprintf(w, "%s\n", err)
			return
		}
		fmt.Fprintf(w, "%s", string(b))
	})

	// Setup capfilter - consumes serviceMux
	capMux := http.NewServeMux()
	capMux.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		user := req.Header.Get("X-Authenticated-Username")
		caps := getUserString(tlsConfig, user, "capabilities")
		req.Header.Set("X-Authenticated-Capability", caps)
		serviceMux.ServeHTTP(w, req)
	})

	// Setup Authfilter - consumes capMux
	var authHandler http.Handler
	if authFilter == "digest" {
		authHandler = NewDigestAuthFilter(capMux, tokenManager, digestRealm, tlsConfig)
	} else if authFilter == "saml" {
		authHandler = NewSamlAuthFilter(capMux, tokenManager,
			certPem, keyPem,
			externalIp+":"+strconv.Itoa(listenPort),
			samlIdpssourl, samlIdpssodescurl, samlIdpcert)
	} else if authFilter == "none" {
		authHandler = capMux
	} else {
		log.Fatal("Unknown authFilter: %v", authFilter)
	}

	// Make token test filter
	tokenMux := http.NewServeMux()
	tokenMux.HandleFunc("/api/license", NewMultipleHostReverseProxy(ServiceRegistry, tlsConfig))
	tokenMux.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		// Handle CORS BS
		addCorsHeader(w, req)
		if req.Method == "OPTIONS" {
			w.Header().Set("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH,HEAD")
			w.WriteHeader(http.StatusNoContent)
			return
		}

		token, err := tokenManager.Get(req)
		if err != nil {
			authHandler.ServeHTTP(w, req)
		} else {
			err = tokenManager.AddTokenInfo(token, w, req)
			if err != nil {
				log.Printf("ati failed: %v\n", err)
			}
			capMux.ServeHTTP(w, req)
		}
	})

	println("starting consul")
	go ServiceRegistry.WatchConsul()

	println("ready")
	log.Fatal(http.ListenAndServeTLS(":"+strconv.Itoa(listenPort), certPem, keyPem, tokenMux))
}

var tokenSecretKey string = "digitalrebar/private/revproxy/token/secret"

func getTokenSecretKey() (string, error) {
	client, err := api.NewClient(api.DefaultConfig())
	if err != nil {
		log.Printf("Failed to create consul client\n")
		return "", err
	}
	if _, err := client.Agent().Self(); err != nil {
		log.Printf("Failed to get consul agent\n")
		return "", err
	}

	store := client.KV()
	pair, _, err := store.Get(tokenSecretKey, nil)
	if err != nil {
		log.Printf("Failed to find: %v\n", tokenSecretKey)
		return "", err
	}
	if pair == nil {
		randString := RandString(32)
		_, err = store.Put(&api.KVPair{Key: tokenSecretKey, Value: []byte(randString)}, nil)
		if err != nil {
			log.Printf("Failed to put: %v\n", tokenSecretKey)
			return "", err
		}
	}

	pair, _, err = store.Get(tokenSecretKey, nil)
	if err != nil {
		log.Printf("Failed to re-get: %v\n", tokenSecretKey)
		return "", err
	}

	return string(pair.Value), nil
}
