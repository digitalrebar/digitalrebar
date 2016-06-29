package main

import (
	"crypto/tls"
	"io/ioutil"
	"log"
	"math/rand"
	"net/http"

	"github.com/kmanley/go-http-auth"
)

func NewDigestAuthFilter(myMux *http.ServeMux, tm *JwtManager,
	digestRealm string, tlsConfig *tls.Config) http.Handler {

	authenticator := auth.NewDigestAuthenticator(digestRealm, func(user, realm string) string {

		tr := &http.Transport{
			TLSClientConfig: tlsConfig,
		}
		client := &http.Client{Transport: tr}
		req, _ := http.NewRequest("GET", "https://127.0.0.1/api/v2/users/"+user+"/digest", nil)

		tag, err := ServiceRegistry.ExtractTag(req.URL)
		if err != nil {
			log.Printf("Failed to extract Tag: %v\n", err)
			return ""
		}
		endpoints, err := ServiceRegistry.LookupTag(tag)
		if err != nil {
			log.Printf("Failed to lookup Tag: %v\n", err)
			return ""
		}

		if len(endpoints) == 0 {
			log.Printf("Failed to find endpoints: %s\n", user)
			return ""
		}

		req.URL.Host = endpoints[rand.Int()%len(endpoints)]
		req.Header.Set("X-Authenticated-Username", "rebar")
		req.Header.Set("X-Authenticated-Capability", "ADMIN")

		resp, err := client.Do(req)
		if err != nil {
			log.Printf("Request failed: %v\n", err)
			return ""
		}
		defer resp.Body.Close()

		htmlData, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			log.Println(err)
			return ""
		}
		return string(htmlData)
	})

	newMux := http.NewServeMux()
	// Override the builtin wrap function to include our capabilities.
	newMux.HandleFunc("/",
		authenticator.Wrap(func(w http.ResponseWriter, ar *auth.AuthenticatedRequest) {
			t := tm.New(ar.Username)
			err := tm.AddTokenInfo(t, w, &ar.Request)
			if err != nil {
				log.Printf("Add token had issues: %v\n", err)
			}

			myMux.ServeHTTP(w, &ar.Request)
		}))

	return newMux
}
