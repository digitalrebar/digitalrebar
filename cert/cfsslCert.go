package cert

import (
	"crypto/tls"
	"encoding/json"

	"github.com/cloudflare/cfssl/api/client"
	"github.com/cloudflare/cfssl/auth"
	"github.com/cloudflare/cfssl/csr"
	"github.com/cloudflare/cfssl/info"
	"github.com/cloudflare/cfssl/signer"
)

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
		KeyRequest: csr.NewBasicKeyRequest(),
		CN:         CN,
		Names:      names,
		Hosts:      hosts,
	}

	g := &csr.Generator{Validator: Validator}
	csrPem, key, err = g.ProcessRequest(&req)
	return
}

func CreateCertificate(remote, authKey, label, CN string, hosts []string) (cert, key []byte, err error) {
	cert = nil
	key = nil
	csrPem, key, err := CreateCSR(label, CN, hosts)
	if err != nil {
		return
	}

	sign := signer.SignRequest{
		Request: string(csrPem),
		Label:   label,
	}

	s := client.NewServerTLS(remote, &tls.Config{InsecureSkipVerify: true})
	testProvider, err := auth.New(authKey, []byte{})
	if err != nil {
		return
	}
	signJSON, _ := json.Marshal(sign)
	cert, err = s.AuthSign(signJSON, []byte{}, testProvider)
	if err != nil {
		return
	}
	return
}
