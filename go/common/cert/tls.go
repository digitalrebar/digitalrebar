package cert

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"os"
)

func simpleKeys(trustRoot, name string) (*x509.CertPool, *tls.Certificate, error) {
	ips, err := net.InterfaceAddrs()
	if err != nil {
		return nil, nil, err
	}
	addrs := []string{name, "localhost"}
	for _, ip := range ips {
		cidr := ip.String()
		ip, _, err := net.ParseCIDR(cidr)
		if err != nil || ip == nil {
			continue
		}
		addrs = append(addrs, ip.String())
	}
	v, c, p, err := GetKeysFor(trustRoot, name, addrs)
	if err != nil {
		return nil, nil, err
	}
	pool := x509.NewCertPool()
	if !pool.AppendCertsFromPEM(v) {
		return nil, nil, fmt.Errorf("Failed to add validator to cert pool: \n%v", string(v))
	}
	cert, err := tls.X509KeyPair(c, p)
	if err != nil {
		return nil, nil, fmt.Errorf("Failed to create TLS keypair from returned certs: %v", err)
	}
	return pool, &cert, nil
}

// Client creates an HTTP client that is pre-configured to use TLS mutual auth
// with Servers that derive their certificates from the same root in trust-me.
//
// trustRoot is the name of the root in the trust-me service that you should create
// the client name for, and clientName is the name the client should use for the certificate.
func Client(trustRoot, clientName string) (*http.Client, error) {
	pool, cert, err := simpleKeys(trustRoot, clientName)
	tlsCfg := &tls.Config{
		RootCAs:      pool,
		Certificates: []tls.Certificate{*cert},
	}
	tlsCfg.BuildNameToCertificate()
	tr := &http.Transport{TLSClientConfig: tlsCfg}
	return &http.Client{Transport: tr}, err
}

// Server returns a preconfigured HTTPS server with keys fetched from trust-me.
// It will be configured to perform full peer cert validation with any clients.
func Server(trustRoot, serverName string) (*http.Server, error) {
	pool, cert, err := simpleKeys(trustRoot, serverName)
	return &http.Server{
		TLSConfig: &tls.Config{
			ClientCAs:  pool,
			ClientAuth: tls.RequireAndVerifyClientCert,
			GetCertificate: func(c *tls.ClientHelloInfo) (*tls.Certificate, error) {
				return cert, nil
			},
		},
	}, err
}

// ServeTLS creates an HTTPS server that authenticates with the specified
// address, handler, and TLS certificate.
func ServeTLS(addr string, handler http.Handler, keypair *tls.Certificate) error {
	s := &http.Server{
		Addr:    addr,
		Handler: handler,
		TLSConfig: &tls.Config{
			GetCertificate: func(c *tls.ClientHelloInfo) (*tls.Certificate, error) {
				return keypair, nil
			},
		},
	}
	return s.ListenAndServeTLS("", "")
}

// ServeTrustedTLS creates an HTTPS server that expects to be able to
// perform TLS mutual authentication with any clients that talk to it
// -- both its certificate and any certificate a client presents to it
// must be signed by a public key in validatorPool.
func ServeTrustedTLS(addr string, handler http.Handler, keypair *tls.Certificate, validatorPool *x509.CertPool) error {
	s := &http.Server{
		Addr:    addr,
		Handler: handler,
		TLSConfig: &tls.Config{
			ClientCAs:  validatorPool,
			ClientAuth: tls.RequireAndVerifyClientCert,
			GetCertificate: func(c *tls.ClientHelloInfo) (*tls.Certificate, error) {
				return keypair, nil
			},
		},
	}
	return s.ListenAndServeTLS("", "")
}

func WriteFiles(certB, keyB []byte) error {
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
	err := WriteFiles(certB, keyB)
	if err != nil {
		return err
	}
	return http.ListenAndServeTLS(addr, ".tlsCache/certfile", ".tlsCache/keyfile", handler)
}

/*
  This is still STUPID, but the only way to do this for now.
*/
func ListenAndServeTLSValidated(addr string, valCertB, certB, keyB []byte, handler http.Handler) error {
	err := WriteFiles(certB, keyB)
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
	trustMeAddr, authKeyB, err := GetTrustMeServiceInfo(sendingRoot)
	if err != nil {
		return fmt.Errorf("Failed to contact trustme service for info: %s  %v\n", sendingRoot, err)
	}

	certB, keyB, err := CreateCertificate(trustMeAddr, string(authKeyB), sendingRoot, CN, hosts)
	if err != nil {
		return fmt.Errorf("Could not create certificate for %s from %v: %v\n", sendingRoot, trustMeAddr, err)
	}

	// If we have a root to validate incoming connections, use it, otherwise
	// just say who we are.
	if acceptingRoot != "" {
		valCertB, err := GetCertificateForRoot(trustMeAddr, acceptingRoot)
		if err != nil {
			return fmt.Errorf("Could not get root certificate for %s from %v: %v\n", acceptingRoot, trustMeAddr, err)
		}

		return ListenAndServeTLSValidated(addr, valCertB, certB, keyB, handler)
	}
	return ListenAndServeTLS(addr, certB, keyB, handler)
}
