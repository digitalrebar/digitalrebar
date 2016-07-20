package cert

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"

	"github.com/digitalrebar/go-common/service"
	"github.com/digitalrebar/go-common/store"

	consul "github.com/hashicorp/consul/api"
)

func writeFiles(certB, keyB []byte) error {
	err := os.MkdirAll(".tlsCache", 0700)
	if err != nil {
		return err
	}

	err = ioutil.WriteFile(".tlsCache/keyfile", keyB, 0600)
	if err != nil {
		return err
	}

	err = ioutil.WriteFile(".tlsCache/certfile", certB, 0600)
	if err != nil {
		return err
	}
	return nil
}

/*
  This is STUPID, but the only way to do this for now.
*/
func ListenAndServeTLS(addr string, certB, keyB []byte, handler http.Handler) error {
	err := writeFiles(certB, keyB)
	if err != nil {
		return err
	}
	return http.ListenAndServeTLS(addr, ".tlsCache/certfile", ".tlsCache/keyfile", handler)
}

/*
  This is still STUPID, but the only way to do this for now.
*/
func ListenAndServeTLSValidated(addr string, valCertB, certB, keyB []byte, handler http.Handler) error {
	err := writeFiles(certB, keyB)
	if err != nil {
		return err
	}

	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM(valCertB)

	// Setup HTTPS client
	tlsConfig := &tls.Config{
		ClientCAs: caCertPool,
		// NoClientCert
		// RequestClientCert
		// RequireAnyClientCert
		// VerifyClientCertIfGiven
		// RequireAndVerifyClientCert
		ClientAuth: tls.RequireAndVerifyClientCert,
	}
	tlsConfig.BuildNameToCertificate()

	s := &http.Server{
		Addr:    addr,
		Handler: handler,
	}
	s.TLSConfig = tlsConfig

	return s.ListenAndServeTLS(".tlsCache/certfile", ".tlsCache/keyfile")
}

func StartTLSServer(addr, CN string, hosts []string, acceptingRoot, sendingRoot string, handler http.Handler) error {
	cc, err := consul.NewClient(consul.DefaultConfig())
	if err != nil {
		return err
	}
	if _, err := cc.Agent().Self(); err != nil {
		return err
	}

	svc, err := service.WaitService(cc, "trust-me", "")
	if err != nil {
		log.Printf("Could not get trust-me service: %v\n", err)
		return err
	}
	trustMeAddr := fmt.Sprintf("%s:%d", svc[0].ServiceAddress, svc[0].ServicePort)

	simpleStore, err := store.NewSimpleConsulStore(cc, "trust-me/cert-store")
	if err != nil {
		log.Printf("Failed to connect to consul: %v\n", err)
		return err
	}
	authKeyB, err := simpleStore.Load(fmt.Sprintf("%s/authkey", sendingRoot))
	if err != nil {
		log.Printf("Could not get authkey for %s: %v\n", sendingRoot, err)
		return err
	}

	certB, keyB, err := CreateCertificate(trustMeAddr, string(authKeyB), sendingRoot, CN, hosts)
	if err != nil {
		log.Printf("Could not create certificate for %s from %v: %v\n", sendingRoot, trustMeAddr, err)
		return err
	}

	// If we have a root to validate incoming connections, use it, otherwise
	// just say who we are.
	if acceptingRoot != "" {
		valCertB, err := GetCertificateForRoot(trustMeAddr, acceptingRoot)
		if err != nil {
			log.Printf("Could not get root certificate for %s from %v: %v\n", acceptingRoot, trustMeAddr, err)
			return err
		}

		return ListenAndServeTLSValidated(addr, valCertB, certB, keyB, handler)
	} else {
		return ListenAndServeTLS(addr, certB, keyB, handler)
	}
}
