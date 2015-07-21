package main

import (
	"github.com/stretchr/testify/assert"
	"net/http"
	"net/http/httptest"
	"testing"
)

func base_url(path string) string {
	return "http://127.0.0.1:6755" + path
}

func get_frontend() (*Frontend, http.Handler) {
	cfg := Config{}
	cfg.Network.Port = 6755
	cfg.Network.Username = "fred"
	cfg.Network.Password = "rules"
	the_fe := NewFrontend(".", "", "", cfg)
	handler := the_fe.RunServer(false)
	return the_fe, handler
}

func TestGetAllSubnets(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets")
	req, err := http.NewRequest("GET", url, nil)
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 200, recorder.Code)
	assert.Equal(t, "[]", recorder.Body.String(), "Expected [], but got %s", recorder.Body.String())
}

func TestGetAllSubnetMissing(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb")
	req, err := http.NewRequest("GET", url, nil)
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 404, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"Not Found\"\n}", recorder.Body.String(), "Expected Not Found, but got %s", recorder.Body.String())
}
