package main

import (
	"crypto/tls"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/assert"
)

func base_url(path string) string {
	return "http://127.0.0.1" + path
}

func init_test() (Registry, *tls.Config) {
	tlsConfig := &tls.Config{}

	return ServiceRegistry, tlsConfig
}

func TestRequestBadAuth(t *testing.T) {
	handler := NewMultipleHostReverseProxy(init_test())

	recorder := httptest.NewRecorder()
	url := base_url("/nodes/dhcp/v1/subnets")
	req, err := http.NewRequest("GET", url, nil)
	assert.Nil(t, err)

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 500, recorder.Code)
	assert.Equal(t, "{\"Error\":\"Not Authorized\"}", recorder.Body.String(), "Expected Not Authorized, but got %s", recorder.Body.String())
}
