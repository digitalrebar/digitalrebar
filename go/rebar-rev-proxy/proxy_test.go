package main

import (
	"crypto/tls"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/assert"
)

func baseUrl(path string) string {
	return "http://127.0.0.1" + path
}

func initTest() (Registry, *tls.Config) {
	tlsConfig := &tls.Config{}

	return ServiceRegistry, tlsConfig
}

func TestRequestBadAuth(t *testing.T) {
	handler := NewMultipleHostReverseProxy(initTest())

	recorder := httptest.NewRecorder()
	url := baseUrl("/nodes/dhcp/v1/subnets")
	req, err := http.NewRequest("GET", url, nil)
	assert.Nil(t, err)

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 500, recorder.Code)
	assert.Equal(t, "{\"Error\":\"Not Authorized\"}", recorder.Body.String(), "Expected Not Authorized, but got %s", recorder.Body.String())
}
