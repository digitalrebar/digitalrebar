package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math/rand"
	"net/http"
	"net/http/httputil"
	"text/template"
	"time"

	"github.com/cloudflare/cfssl/api"
	"github.com/cloudflare/cfssl/auth"
	"github.com/cloudflare/cfssl/config"
	"github.com/cloudflare/cfssl/csr"
	"github.com/cloudflare/cfssl/helpers"
	"github.com/cloudflare/cfssl/initca"
	"github.com/cloudflare/cfssl/log"
	"github.com/cloudflare/cfssl/signer"
	"github.com/cloudflare/cfssl/signer/local"
	"github.com/cloudflare/cfssl/whitelist"
	metrics "github.com/cloudflare/go-metrics"
)

// A SignatureResponse contains only a certificate, as there is no other
// useful data for the CA to return at this time.
type SignatureResponse struct {
	Certificate string `json:"certificate"`
}

type filter func(string, *signer.SignRequest) bool

var filters = map[string][]filter{}

type signerStats struct {
	Counter metrics.Counter
	Rate    metrics.Meter
}

var stats struct {
	Registry         metrics.Registry
	Requests         map[string]signerStats
	TotalRequestRate metrics.Meter
	ErrorPercent     metrics.GaugeFloat64
	ErrorRate        metrics.Meter
}

func initStatBucket(k string) {
	if _, ok := stats.Requests[k]; ok {
		return
	}
	stats.Requests[k] = signerStats{
		Counter: metrics.NewRegisteredCounter("requests:"+k, stats.Registry),
		Rate:    metrics.NewRegisteredMeter("request-rate:"+k, stats.Registry),
	}
}

func initStats() {
	stats.Registry = metrics.NewRegistry()

	stats.Requests = map[string]signerStats{}

	// signers is defined in ca.go
	for k := range signers {
		initStatBucket(k)
	}

	stats.TotalRequestRate = metrics.NewRegisteredMeter("total-request-rate", stats.Registry)
	stats.ErrorPercent = metrics.NewRegisteredGaugeFloat64("error-percent", stats.Registry)
	stats.ErrorRate = metrics.NewRegisteredMeter("error-rate", stats.Registry)
}

// incError increments the error count and updates the error percentage.
func incErrors() {
	stats.ErrorRate.Mark(1)
	eCtr := float64(stats.ErrorRate.Count())
	rCtr := float64(stats.TotalRequestRate.Count())
	stats.ErrorPercent.Update(eCtr / rCtr * 100)
}

// incRequests increments the request count and updates the error percentage.
func incRequests() {
	stats.TotalRequestRate.Mark(1)
	eCtr := float64(stats.ErrorRate.Count())
	rCtr := float64(stats.TotalRequestRate.Count())
	stats.ErrorPercent.Update(eCtr / rCtr * 100)
}

func fail(w http.ResponseWriter, req *http.Request, status, code int, msg, ad string) {
	incErrors()

	if ad != "" {
		ad = " (" + ad + ")"
	}
	log.Errorf("[HTTP %d] %d - %s%s", status, code, msg, ad)

	dumpReq, err := httputil.DumpRequest(req, true)
	if err != nil {
		fmt.Printf("%v#v\n", req)
	} else {
		fmt.Printf("%s\n", dumpReq)
	}

	res := api.NewErrorResponse(msg, code)
	w.WriteHeader(status)
	jenc := json.NewEncoder(w)
	jenc.Encode(res)
}

func dispatchRequest(w http.ResponseWriter, req *http.Request) {
	incRequests()

	if req.Method != "POST" {
		fail(w, req, http.StatusMethodNotAllowed, 1, "only POST is permitted", "")
		return
	}

	body, err := ioutil.ReadAll(req.Body)
	if err != nil {
		fail(w, req, http.StatusInternalServerError, 1, err.Error(), "while reading request body")
		return
	}
	defer req.Body.Close()

	var authReq auth.AuthenticatedRequest
	err = json.Unmarshal(body, &authReq)
	if err != nil {
		fail(w, req, http.StatusBadRequest, 1, err.Error(), "while unmarshaling request body")
		return
	}

	var sigRequest signer.SignRequest
	err = json.Unmarshal(authReq.Request, &sigRequest)
	if err != nil {
		fail(w, req, http.StatusBadRequest, 1, err.Error(), "while unmarshalling authenticated request")
		return
	}

	if sigRequest.Label == "" {
		sigRequest.Label = defaultLabel
	}

	acl := whitelists[sigRequest.Label]
	if acl != nil {
		ip, err := whitelist.HTTPRequestLookup(req)
		if err != nil {
			fail(w, req, http.StatusInternalServerError, 1, err.Error(), "while getting request IP")
			return
		}

		if !acl.Permitted(ip) {
			fail(w, req, http.StatusForbidden, 1, "not authorised", "because IP is not whitelisted")
			return
		}
	}

	s, ok := signers[sigRequest.Label]
	if !ok {
		fail(w, req, http.StatusBadRequest, 1, "bad request", "request is for non-existent label "+sigRequest.Label)
		return
	}

	initStatBucket(sigRequest.Label)
	stats.Requests[sigRequest.Label].Counter.Inc(1)
	stats.Requests[sigRequest.Label].Rate.Mark(1)

	// Sanity checks to ensure that we have a valid policy. This
	// should have been checked in NewAuthSignHandler.
	policy := s.Policy()
	if policy == nil {
		fail(w, req, http.StatusInternalServerError, 1, "invalid policy", "signer was initialised without a signing policy")
		return
	}
	profile := policy.Default

	if policy.Profiles != nil && sigRequest.Profile != "" {
		profile = policy.Profiles[sigRequest.Profile]
		if profile == nil {
			fail(w, req, http.StatusBadRequest, 1, "invalid profile", "failed to look up profile with name: "+sigRequest.Profile)
			return
		}
	}

	if profile == nil {
		fail(w, req, http.StatusInternalServerError, 1, "invalid profile", "signer was initialised without any valid profiles")
		return
	}

	if profile.Provider == nil {
		fail(w, req, http.StatusUnauthorized, 1, "authorisation required", "received unauthenticated request")
		return
	}

	if !profile.Provider.Verify(&authReq) {
		fail(w, req, http.StatusBadRequest, 1, "invalid token", "received authenticated request with invalid token")
		return
	}

	if sigRequest.Request == "" {
		fail(w, req, http.StatusBadRequest, 1, "invalid request", "empty request")
		return
	}

	cert, err := s.Sign(sigRequest)
	if err != nil {
		fail(w, req, http.StatusBadRequest, 1, "bad request", "signature failed: "+err.Error())
		return
	}

	x509Cert, err := helpers.ParseCertificatePEM(cert)
	if err != nil {
		fail(w, req, http.StatusInternalServerError, 1, "bad certificate", err.Error())
	}

	log.Infof("signature: requester=%s, label=%s, profile=%s, serialno=%s",
		req.RemoteAddr, sigRequest.Label, sigRequest.Profile, x509Cert.SerialNumber)

	res := api.NewSuccessResponse(&SignatureResponse{Certificate: string(cert)})
	jenc := json.NewEncoder(w)
	err = jenc.Encode(res)
	if err != nil {
		log.Errorf("error writing response: %v", err)
	}
}

func metricsDisallowed(w http.ResponseWriter, req *http.Request) {
	log.Warning("attempt to access metrics endpoint from external address ", req.RemoteAddr)
	http.NotFound(w, req)
}

func dumpMetrics(w http.ResponseWriter, req *http.Request) {
	log.Info("whitelisted requested for metrics endpoint")
	var statsOut = struct {
		Metrics metrics.Registry `json:"metrics"`
		Signers []string         `json:"signers"`
	}{stats.Registry, make([]string, 0, len(signers))}

	for signer := range signers {
		statsOut.Signers = append(statsOut.Signers, signer)
	}

	out, err := json.Marshal(statsOut)
	if err != nil {
		log.Errorf("failed to dump metrics: %v", err)
	}

	w.Write(out)
}

var (
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
      "key": "{{.AuthKey}}",
      "type": "standard"
    }
  }
}
	`
)

func buildSigner(label, authKey string, certB, keyB, templateB []byte) (signer.Signer, error) {
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

	c, err := config.LoadConfig(templateB)
	if err != nil {
		return nil, err
	}
	s.SetPolicy(c.Signing)

	err = storeSigner(label, authKey, certB, keyB, templateB)
	if err != nil {
		return nil, err
	}

	return s, nil
}

func newRootCertificate(label, authKey string) error {
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
	certB, _, keyB, err := initca.New(&req)
	if err != nil {
		return err
	}

	type TempData struct {
		AuthKey string
	}
	var tempData = TempData{
		AuthKey: authKey,
	}
	t := template.Must(template.New("template").Parse(basicConfigTemplate))
	var doc bytes.Buffer
	err = t.Execute(&doc, tempData)
	if err != nil {
		return err
	}
	templateB := []byte(doc.String())

	s, err := buildSigner(label, authKey, certB, keyB, templateB)
	if err != nil {
		return err
	}
	signers[label] = s
	return nil
}

type newRootData struct {
	Label   string `json:"label"`
	AuthKey string `json:"auth_key,omitempty"`
}

const letterBytes = "0123456789"
const (
	letterIdxBits = 4                    // 4 bits to represent a letter index
	letterIdxMask = 1<<letterIdxBits - 1 // All 1-bits, as many as letterIdxBits
	letterIdxMax  = 63 / letterIdxBits   // # of letter indices fitting in 63 bits
)

var src = rand.NewSource(time.Now().UnixNano())

func RandString(n int) string {
	b := make([]byte, n)
	// A src.Int63() generates 63 random bits, enough for letterIdxMax characters!
	for i, cache, remain := n-1, src.Int63(), letterIdxMax; i >= 0; {
		if remain == 0 {
			cache, remain = src.Int63(), letterIdxMax
		}
		if idx := int(cache & letterIdxMask); idx < len(letterBytes) {
			b[i] = letterBytes[idx]
			i--
		}
		cache >>= letterIdxBits
		remain--
	}

	return string(b)
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

	// If AuthKey is null, create one
	if t.AuthKey == "" {
		t.AuthKey = RandString(32)
	}

	err = newRootCertificate(t.Label, t.AuthKey)
	if err != nil {
		return
	}

	w.WriteHeader(http.StatusOK)
}
