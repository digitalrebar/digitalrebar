package cert

import (
	"io/ioutil"
	"net/http"
	"os"
)

/*
  This is STUPID, but the only way to do this for now.
*/
func ListenAndServeTLS(addr string, certB, keyB []byte, handler http.Handler) error {
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

	return http.ListenAndServeTLS(addr, ".tlsCache/certfile", ".tlsCache/keyfile", handler)
}
