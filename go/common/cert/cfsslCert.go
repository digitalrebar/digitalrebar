package cert

import (
	"crypto/tls"
	"encoding/json"
	"fmt"
	"time"

	"github.com/cloudflare/cfssl/api/client"
	"github.com/cloudflare/cfssl/auth"
	"github.com/cloudflare/cfssl/csr"
	"github.com/cloudflare/cfssl/info"
	cfssllog "github.com/cloudflare/cfssl/log"
	"github.com/cloudflare/cfssl/signer"
	"github.com/digitalrebar/go-common/service"
	"github.com/digitalrebar/go-common/store"
	consul "github.com/hashicorp/consul/api"
)

// GetTrustMeServiceInfo fetches the service information and the
// signing key for the desired trust root for the trust-me service
// from a local consul instance, and returns the endpoint for the
// trust-me service along with a key to pass along to sign any key
// requests made to that trust-me instance.  Digital Rebar services
// inside the trust zone can use "internal" as the trustRoot string.
func GetTrustMeServiceInfo(trustRoot string) (endpoint string, signingKey []byte, err error) {
	consulClient, err := consul.NewClient(consul.DefaultConfig())
	if err != nil {
		return "", nil, fmt.Errorf("Could not talk to Consul: %v", err)
	}
	svc := []*consul.CatalogService{}
	for {
		svc, err = service.Find(consulClient, "trust-me", "")
		if err != nil {
			return "", nil, fmt.Errorf("Could not get trust-me service: %v\n", err)
		}
		if len(svc) > 0 {
			break
		}
	}
	addr, port := service.Address(svc[0])
	endpoint = fmt.Sprintf("https://%s:%d", addr, port)

	simpleStore, err := store.NewSimpleConsulStore(consulClient, "trust-me/cert-store")
	if err != nil {
		return "", nil, fmt.Errorf("Failed to connect to consul: %v\n", err)
	}

	count := 0
retry:
	signingKey, err = simpleStore.Load(fmt.Sprintf("%s/authkey", trustRoot))
	if err != nil {
		if count > 30 {
			return "", nil, err
		}
		count += 1
		time.Sleep(5 * time.Second)
		goto retry
	}
	return
}

func GetCertificateForRoot(remote, label string) (cert []byte, err error) {
	req := new(info.Req)
	req.Label = label

	serv := client.NewServerTLS(remote, &tls.Config{InsecureSkipVerify: true})
	reqJSON, _ := json.Marshal(req)
	resp, err := serv.Info(reqJSON)
	if err != nil {
		return
	}

	cert = []byte(resp.Certificate)
	return
}

// Validator does nothing and will never return an error. It exists because creating a
// csr.Generator requires a Validator.
func Validator(req *csr.CertificateRequest) error {
	return nil
}

func CreateCSR(label, CN string, hosts []string) (csrPem, key []byte, err error) {
	csrPem = nil
	key = nil

	// Make CSR for this host
	names := make([]csr.Name, 0, 0)
	name := csr.Name{
		C:  "US",
		ST: "Texas",
		L:  "Austin",
		O:  "RackN",
		OU: "CA Services",
	}
	names = append(names, name)
	req := csr.CertificateRequest{
		KeyRequest: &csr.BasicKeyRequest{"rsa", 2048},
		CN:         CN,
		Names:      names,
		Hosts:      hosts,
	}

	g := &csr.Generator{Validator: Validator}
	csrPem, key, err = g.ProcessRequest(&req)
	return
}

// CreateCertificate uses trust-me to create a new certificate and
// private key for local use.  remote and authkey are the values
// recieved from cert.GetTrustMeServiceInfo, label is the name that
// was passed to cert.GetTrustMeServiceInfo, CN is the canonical name
// of the cert to be created, and hosts is a list of hostnames and IP
// addresses that the generated certificate should be valid for.
func CreateCertificate(remote, authKey, label, CN string, hosts []string) (cert, key []byte, err error) {
	cert = nil
	key = nil
	var csrPem []byte
	csrPem, key, err = CreateCSR(label, CN, hosts)
	if err != nil {
		return nil, nil, fmt.Errorf("Error in CreateCSR: %v", err)
	}

	sign := signer.SignRequest{
		Request: string(csrPem),
		Label:   label,
	}

	s := client.NewServerTLS(remote, &tls.Config{InsecureSkipVerify: true})
	testProvider, err := auth.New(authKey, []byte{})
	if err != nil {
		return nil, nil, fmt.Errorf("Error contacting trust-me service: %v", err)
	}
	signJSON, _ := json.Marshal(sign)
	cert, err = s.AuthSign(signJSON, []byte{}, testProvider)
	if err != nil {
		return nil, nil, fmt.Errorf("Error signing key: %v", err)
	}
	return
}

// GetKeysFor is a helper function that wraps GetTrustMeServiceInfo and CreateCertificate
// to simplify the wrapped certificate creation process.
//
// Parameters: trustRoot is the root CA that the newly-created keys
// should be signed by, CN os the canonical name of the cert to be
// created, hosts is a list of names and IP addresses the cert should
// be valid for.
//
// Returns: validator is the certificate that can be used to validate
// that keys were signed by the requested trust root, and cert and
// privateKeys are the public and private keypairs that were
// generated.
func GetKeysFor(trustRoot string, CN string, hosts []string) (validator, cert, privateKey []byte, err error) {
	cfssllog.Level = cfssllog.LevelError
	var ep string
	ep, token, err := GetTrustMeServiceInfo(trustRoot)
	if err != nil {
		return
	}
	cert, privateKey, err = CreateCertificate(ep, string(token), trustRoot, CN, hosts)
	if err != nil {
		return
	}
	validator, err = GetCertificateForRoot(ep, trustRoot)
	return
}
