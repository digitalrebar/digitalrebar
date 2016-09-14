package main

import (
	"errors"
	"html/template"
	"log"
	"net/http"
	"os"

	"github.com/RobotsAndPencils/go-saml"
	"github.com/rackn/digitalrebar/go/common/cert"
)

type SamlAuthFilter struct {
	*http.ServeMux
	saml.ServiceProviderSettings
	cache   map[string]*http.Request
	nextMux *http.ServeMux
	tm      *JwtManager
}

func NewSamlAuthFilter(mux *http.ServeMux, tm *JwtManager,
	certB []byte,
	keyB []byte,
	myIpport string,
	idpssourl string,
	idpssodescurl string,
	idpcert string) *SamlAuthFilter {

	err := cert.WriteFiles(certB, keyB)
	if err != nil {
		log.Printf("Failed to write cert files")
		return nil
	}

	saf := &SamlAuthFilter{
		nextMux: mux,
		tm:      tm,
		cache:   make(map[string]*http.Request),

		ServiceProviderSettings: saml.ServiceProviderSettings{
			PublicCertPath:              ".tlsCache/certfile",
			PrivateKeyPath:              ".tlsCache/keyfile",
			IDPSSOURL:                   idpssourl,
			IDPSSODescriptorURL:         idpssodescurl,
			IDPPublicCertPath:           idpcert,
			SPSignRequest:               true,
			AssertionConsumerServiceURL: "https://" + myIpport + "/samlresponse",
		},
	}

	saf.ServeMux = http.NewServeMux()
	saf.ServeMux.HandleFunc("/", saf.generateSamlRequest)
	saf.ServeMux.HandleFunc("/samlresponse", saf.handleSamlResponse)

	saf.ServiceProviderSettings.Init()

	log.Println("SAML Filter built")
	log.Printf(" - %15v = %v\n", "keyPath", ".tlsCache/keyfile")
	log.Printf(" - %15v = %v\n", "certPath", ".tlsCache/certfile")
	log.Printf(" - %15v = %v\n", "idpssourl", idpssourl)
	log.Printf(" - %15v = %v\n", "idpssodescurl", idpssodescurl)
	log.Printf(" - %15v = %v\n", "idpcert", idpcert)
	return saf
}

func (saf *SamlAuthFilter) generateSamlRequest(w http.ResponseWriter, req *http.Request) {
	// generate the AuthnRequest and then get a base64 encoded string of the XML
	authnRequest := saf.GetAuthnRequest()
	b64XML, err := authnRequest.EncodedSignedString(saf.PrivateKeyPath)
	if err != nil {
		panic(err)
	}

	// for convenience, get a URL formed with the SAMLRequest parameter
	url, err := saml.GetAuthnRequestURL(saf.IDPSSOURL, b64XML, "fred")
	if err != nil {
		panic(err)
	}

	// Store the req for later
	saf.cache[authnRequest.ID] = req

	// below is bonus for how you might respond to a
	// request with a form that POSTs to the IdP
	data := struct {
		Base64AuthRequest string
		URL               string
	}{
		Base64AuthRequest: b64XML,
		URL:               url,
	}

	// Add data as header for CLIs
	w.Header().Set("DR-SAML-URL", data.URL)
	w.Header().Set("DR-SAML-REQ", data.Base64AuthRequest)

	t := template.New("saml")
	t, err = t.Parse("<html><body style=\"display: none\" onload=\"document.frm.submit()\"><form method=\"post\" name=\"frm\" action=\"{{.URL}}\"><input type=\"hidden\" name=\"SAMLRequest\" value=\"{{.Base64AuthRequest}}\" /><input type=\"submit\" value=\"Submit\" /></form></body></html>")

	// how you might respond to a request with the templated form that will auto post
	t.Execute(w, data)
}

func (saf *SamlAuthFilter) validateSamlResponse(encodedXML string) (*string, *string, error) {
	if encodedXML == "" {
		return nil, nil, errors.New("SAMLResponse form value missing")
	}

	response, err := saml.ParseEncodedResponse(encodedXML)
	if err != nil {
		return nil, nil, err
	}

	err = response.Validate(&saf.ServiceProviderSettings)
	if err != nil {
		return &response.InResponseTo, nil, err
	}

	samlID := response.GetAttribute("name")
	if samlID == "" {
		return &response.InResponseTo, nil, errors.New("SAML attribute name missing\n")
	}

	return &response.InResponseTo, &samlID, nil
}

func (saf *SamlAuthFilter) handleSamlResponse(w http.ResponseWriter, r *http.Request) {
	println("handle SAML Response")

	samlResponse := r.FormValue("SAMLResponse")

	id, samlID, err := saf.validateSamlResponse(samlResponse)
	if err != nil {
		println("SAML Validation Failed")
		if err.Error() == "SAMLResponse form value missing" {
			println("Just missing data - generate")
			saf.generateSamlRequest(w, r)
		} else {
			println("Bad data or invalid data - error it:", err.Error())
			w.WriteHeader(500)
			w.Write([]byte(err.Error()))
		}
		return
	}

	println("handle SAML Response - Valid Response")
	// Get original req
	req, ok := saf.cache[*id]
	if !ok {
		println("handle SAML Response - Failed to find cached request")
		w.WriteHeader(500)
		w.Write([]byte("Couldn't find cached request"))
		return
	}
	delete(saf.cache, *id)

	println("Authorized by SAML: " + *samlID)

	t := saf.tm.New(*samlID)
	err = saf.tm.AddTokenInfo(t, w, req)
	if err != nil {
		log.Printf("Add Token info had issues: %v\n", err)
	}

	req.Write(os.Stdout)

	http.Redirect(w, req, req.URL.String(), 301)
}
