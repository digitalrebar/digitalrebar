package main

import (
	"encoding/json"
	"flag"
	"log"
	"net"
	"net/http"

	"github.com/digitalrebar/go-common/cert"

	"github.com/cloudflare/cfssl/api/info"
	"github.com/cloudflare/cfssl/config"
	"github.com/cloudflare/cfssl/helpers"
	"github.com/cloudflare/cfssl/signer"
	"github.com/cloudflare/cfssl/signer/local"
	"github.com/cloudflare/cfssl/whitelist"
)

var (
	defaultLabel        string
	signers                    = map[string]signer.Signer{}
	whitelists                 = map[string]whitelist.NetACL{}
	basicConfigTemplate string = `
{
  "signing": {
    "profiles": {
      "CA": {
        "auth_key": "key1",
        "expiry": "8760h",
        "usages": [
          "signing",
          "key encipherment",
          "server auth",
          "client auth"
        ]
      }
    },
    "default": {
      "auth_key": "key1",
      "expiry": "8760h",
      "usages": [
        "signing",
        "key encipherment",
        "server auth",
        "client auth"
      ]
    }
  },
  "auth_keys": {
    "key1": {
      "key": "12345678",
      "type": "standard"
    }
  }
}
	`
)

func buildSigner(label string) (signer.Signer, error) {
	certB, keyB, err := cert.CreateCertificateRoot(label)
	if err != nil {
		return nil, err
	}

	key, err := helpers.ParsePrivateKeyPEM(keyB)
	if err != nil {
		return nil, err
	}

	cert, err := helpers.ParseCertificatePEM(certB)
	if err != nil {
		return nil, err
	}

	s, err := local.NewSigner(key, cert, signer.DefaultSigAlgo(key), nil)
	if err != nil {
		return nil, err
	}

	c, err := config.LoadConfig([]byte(basicConfigTemplate))
	if err != nil {
		return nil, err
	}
	s.SetPolicy(c.Signing)

	return s, nil
}

type newRootData struct {
	Label string `json:"label"`
}

func newRoot(w http.ResponseWriter, req *http.Request) {
	incRequests()
	if req.Method != "POST" {
		fail(w, req, http.StatusMethodNotAllowed, 1, "only POST is permitted", "")
		return
	}

	decoder := json.NewDecoder(req.Body)
	var t newRootData
	err := decoder.Decode(&t)
	if err != nil {
		fail(w, req, http.StatusBadRequest, 1, "Failed to parse body", "")
		return
	}

	if _, ok := signers[t.Label]; ok {
		fail(w, req, http.StatusConflict, 1, "Already exists:"+t.Label, "")
		return
	}

	s, err := buildSigner(t.Label)
	if err != nil {
		log.Printf("Failed to create signer: %s: %v\n", t.Label, err)
		fail(w, req, http.StatusBadRequest, 1, "Failed to create signer", "")
		return
	}
	signers[t.Label] = s

	w.WriteHeader(http.StatusOK)
}

func main() {
	flagAddr := flag.String("a", ":8888", "listening address")
	flagEndpointCert := flag.String("tls-cert", "", "server certificate")
	flagEndpointKey := flag.String("tls-key", "", "server private key")
	flag.Parse()

	defaultLabel = "internal"

	initStats()

	infoHandler, err := info.NewMultiHandler(signers, defaultLabel)
	if err != nil {
		log.Fatal("%v", err)
	}

	var localhost = whitelist.NewBasic()
	localhost.Add(net.ParseIP("127.0.0.1"))
	localhost.Add(net.ParseIP("::1"))
	metrics, err := whitelist.NewHandlerFunc(dumpMetrics, metricsDisallowed, localhost)
	if err != nil {
		log.Fatal("failed to set up the metrics whitelist: %v", err)
	}

	http.HandleFunc("/api/v1/cfssl/authsign", dispatchRequest)
	http.Handle("/api/v1/cfssl/info", infoHandler)
	http.Handle("/api/v1/cfssl/metrics", metrics)
	http.HandleFunc("/api/v1/cfssl/root", newRoot)

	if *flagEndpointCert == "" && *flagEndpointKey == "" {
		log.Println("Now listening on ", *flagAddr)
		log.Fatal(http.ListenAndServe(*flagAddr, nil))
	} else {

		log.Println("Now listening on https:// ", *flagAddr)
		log.Fatal(http.ListenAndServeTLS(*flagAddr, *flagEndpointCert, *flagEndpointKey, nil))
	}

}
