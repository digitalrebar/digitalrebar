package main

import (
	"errors"
	"fmt"
	"github.com/RobotsAndPencils/go-saml"
	"html/template"
	"net/http"
	"os"
)

type SamlAuthFilter struct {
	*http.ServeMux
	saml.ServiceProviderSettings
	cache   map[string]*http.Request
	nextMux *http.ServeMux
}

func NewSamlAuthFilter(mux *http.ServeMux,
	cert_path string,
	key_path string) *SamlAuthFilter {

	saf := &SamlAuthFilter{
		nextMux: mux,
		cache:   make(map[string]*http.Request),

		ServiceProviderSettings: saml.ServiceProviderSettings{
			PublicCertPath:              cert_path,
			PrivateKeyPath:              key_path,
			IDPSSOURL:                   "https://dev-888522.oktapreview.com/app/rackndev888522_rackn_1/exk5ui8zc112R5ioP0h7/sso/saml",
			IDPSSODescriptorURL:         "http://www.okta.com/exk5ui8zc112R5ioP0h7",
			IDPPublicCertPath:           "idpcert.crt",
			SPSignRequest:               true,
			AssertionConsumerServiceURL: "https://localhost:9090/login",
		},
	}

	saf.ServeMux = http.NewServeMux()
	saf.ServeMux.HandleFunc("/", saf.Filter)
	saf.ServeMux.HandleFunc("/login", saf.handleSamlResponse)

	saf.ServiceProviderSettings.Init()

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
	samlResponse := r.FormValue("SAMLResponse")

	id, samlID, err := saf.validateSamlResponse(samlResponse)
	if err != nil {
		if err.Error() == "SAMLResponse form value missing" {
			saf.generateSamlRequest(w, r)
		} else {
			w.WriteHeader(500)
			w.Write([]byte(err.Error()))
		}
		return
	}

	// Get original req
	req, ok := saf.cache[*id]
	if !ok {
		w.WriteHeader(500)
		w.Write([]byte("Couldn't find cached request"))
		return
	}
	delete(saf.cache, *id)

	println("Authorized: " + *samlID)

	// Put information in place
	cookie := http.Cookie{Name: "RackNSamlData", Value: samlResponse}
	http.SetCookie(w, &cookie)
	w.Header().Set("Authorization", samlResponse)

	req.Write(os.Stdout)

	saf.nextMux.ServeHTTP(w, req)
}

func (saf *SamlAuthFilter) Filter(w http.ResponseWriter, r *http.Request) {
	saml_auth := r.Header.Get("Authorization")
	var saml_cookie *string = nil

	for _, cookie := range r.Cookies() {
		fmt.Printf("%v: %v\n", cookie.Name, cookie.Value)
		if cookie.Name == "RackNSamlData" {
			saml_cookie = &cookie.Value
		}
	}

	if saml_auth == "" &&
		(saml_cookie == nil || *saml_cookie == "") {
		saf.generateSamlRequest(w, r)
		return
	}

	if saml_auth != "" {
		_, name, err := saf.validateSamlResponse(saml_auth)
		if err != nil {
			println("Invalid token: " + err.Error())
			saf.generateSamlRequest(w, r)
			return
		}
		println("Authorized: " + *name)
		w.Header().Set("Authorization", saml_auth)
	} else if saml_cookie != nil && *saml_cookie != "" {
		_, name, err := saf.validateSamlResponse(*saml_cookie)
		if err != nil {
			println("Invalid token: " + err.Error())
			saf.generateSamlRequest(w, r)
			return
		}
		println("Authorized: " + *name)
		cookie := http.Cookie{Name: "RackNSamlData", Value: *saml_cookie}
		http.SetCookie(w, &cookie)
	}

	saf.nextMux.ServeHTTP(w, r)
}
