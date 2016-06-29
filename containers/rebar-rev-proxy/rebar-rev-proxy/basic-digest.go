package main

import (
	"log"
	"net/http"

	"github.com/kmanley/go-http-auth"
)

func NewDigestAuthFilter(myMux *http.ServeMux, tm *JwtManager,
	digestRealm string,
	sp auth.SecretProvider,
	cp CapabilityProvider) http.Handler {

	authenticator := auth.NewDigestAuthenticator(digestRealm, sp)
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

func NewBasicAuthFilter(myMux *http.ServeMux, tm *JwtManager,
	digestRealm string,
	sp auth.SecretProvider,
	cp CapabilityProvider) http.Handler {

	authenticator := auth.NewBasicAuthenticator(digestRealm, sp)
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
