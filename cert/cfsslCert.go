package cert

import (
	"github.com/cloudflare/cfssl/api/client"
	"github.com/cloudflare/cfssl/auth"
	"github.com/cloudflare/cfssl/csr"
	"github.com/cloudflare/cfssl/info"
	"github.com/cloudflare/cfssl/initca"
	"github.com/cloudflare/cfssl/signer"

	"encoding/json"
)

func GetCertificateForRoot(remote, label string) (cert []byte, err error) {
	req := new(info.Req)
	req.Label = label

	serv := client.NewServer(remote)
	reqJSON, _ := json.Marshal(req)
	resp, err := serv.Info(reqJSON)
	if err != nil {
		return
	}

	cert = []byte(resp.Certificate)
	return
}

/*
This can be used anywhere, but it it really used by multi-root CA to dynamically build
a new root.  This is a local call.
*/
func CreateCertificateRoot(label string) (cert, key []byte, err error) {

	// Make CSR for this label - use label as CN
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
		CN:         label,
		Names:      names,
	}

	cert, _, key, err = initca.New(&req)
	return
}

// Validator does nothing and will never return an error. It exists because creating a
// csr.Generator requires a Validator.
func Validator(req *csr.CertificateRequest) error {
	return nil
}

func CreateCertificate(remote, authKey, label, CN string, hosts []string) (cert, key []byte, err error) {
	cert = nil
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
	csrPem, key, err := g.ProcessRequest(&req)
	if err != nil {
		return
	}

	sign := signer.SignRequest{
		Request: string(csrPem),
		Label:   label,
	}

	s := client.NewServer(remote)
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
