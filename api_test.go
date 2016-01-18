package main

import (
	"log"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func base_url(path string) string {
	return "http://127.0.0.1:6755" + path
}

func get_frontend() (*Frontend, http.Handler) {
	cfg := Config{}
	cfg.Network.Port = 6755
	cfg.Network.Username = "fred"
	cfg.Network.Password = "rules"
	fs, err := NewFileStore("./database.test.json")
	if err != nil {
		log.Panic(err)
	}
	the_fe := NewFrontend("", "", cfg, fs)
	handler := the_fe.RunServer(false)
	return the_fe, handler
}

func TestRequestBadAuth(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets")
	req, err := http.NewRequest("GET", url, nil)
	assert.Nil(t, err)
	req.SetBasicAuth("greg", "drools")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 401, recorder.Code)
	assert.Equal(t, "{\"Error\":\"Not Authorized\"}", recorder.Body.String(), "Expected Not Authorized, but got %s", recorder.Body.String())
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

// GREG: Add getall with a subnet

func TestGetSubnetMissing(t *testing.T) {
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

// GREG: Add get with a subnet

func TestCreateSubnetBadConvert(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets")
	req, err := http.NewRequest("POST", url, strings.NewReader("{ \"name\": \"ffeedd\", \"subnet\": \"192k.168.124.0/24\", \"active_start\": \"192.168.124.10\", \"active_end\": \"192.168.124.100\"}"))
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"invalid CIDR address: 192k.168.124.0/24\"\n}", recorder.Body.String(), "Expected invalid CIDR address: 192k.168.124.0/24, but got %s", recorder.Body.String())
}

func TestCreateSubnetBadJson(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets")
	req, err := http.NewRequest("POST", url, strings.NewReader("greg"))
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"invalid character 'g' looking for beginning of value\"\n}", recorder.Body.String(), "Expected invalid character 'g' looking for beginning of value, but got %s", recorder.Body.String())
}

func TestCreateSubnetNoPayload(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets")
	req, err := http.NewRequest("POST", url, nil)
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"Must have body\"\n}", recorder.Body.String(), "Expected Must have body, but got %s", recorder.Body.String())
}

// GREG: Add create test

func TestUpdateSubnetNoPayload(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb")
	req, err := http.NewRequest("PUT", url, nil)
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"Must have body\"\n}", recorder.Body.String(), "Expected Must have body, but got %s", recorder.Body.String())
}

func TestUpdateSubnetBadConvert(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb")
	req, err := http.NewRequest("PUT", url, strings.NewReader("{ \"name\": \"ffeedd\", \"subnet\": \"192k.168.124.0/24\", \"active_start\": \"192.168.124.10\", \"active_end\": \"192.168.124.100\"}"))
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"invalid CIDR address: 192k.168.124.0/24\"\n}", recorder.Body.String(), "Expected invalid CIDR address: 192k.168.124.0/24, but got %s", recorder.Body.String())
}

func TestUpdateSubnetBadJson(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb")
	req, err := http.NewRequest("PUT", url, strings.NewReader("greg"))
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"invalid character 'g' looking for beginning of value\"\n}", recorder.Body.String(), "Expected invalid character 'g' looking for beginning of value, but got %s", recorder.Body.String())
}

func TestUpdateSubnetMissing(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb")
	req, err := http.NewRequest("PUT", url, strings.NewReader("{ \"name\": \"ffeedd\", \"subnet\": \"192.168.124.0/24\", \"active_start\": \"192.168.124.10\", \"active_end\": \"192.168.124.100\"}"))
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 404, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"Not Found\"\n}", recorder.Body.String(), "Expected Not Found, but got %s", recorder.Body.String())
}

// GREG: Add update test

func TestDeleteSubnetMissing(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb")
	req, err := http.NewRequest("DELETE", url, nil)
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

// GREG: Add delete test

func TestPostBindingNoPayload(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb/bind")
	req, err := http.NewRequest("POST", url, nil)
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"Must have body\"\n}", recorder.Body.String(), "Expected Must have body, but got %s", recorder.Body.String())
}

func TestPostBindingBadJson(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb/bind")
	req, err := http.NewRequest("POST", url, strings.NewReader("greg"))
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"invalid character 'g' looking for beginning of value\"\n}", recorder.Body.String(), "Expected invalid character 'g' looking for beginning of value, but got %s", recorder.Body.String())
}

func TestPostBindingSubnetMissing(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb/bind")
	req, err := http.NewRequest("POST", url, strings.NewReader("{}"))
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 404, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"Not Found\"\n}", recorder.Body.String(), "Expected Not Found, but got %s", recorder.Body.String())
}

// GREG: Add bind test

func TestDeleteBindingSubnetMissing(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb/bind/aa:ee:ff:11:32:22")
	req, err := http.NewRequest("DELETE", url, nil)
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 404, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"Subnet Not Found\"\n}", recorder.Body.String(), "Expected Subnet Not Found, but got %s", recorder.Body.String())
}

// GREG: Add missing mac bind delete
// GREG: Add bind delete

func TestPutNextServerNoPayload(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb/next_server/1.1.1.1")
	req, err := http.NewRequest("PUT", url, nil)
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"Must have body\"\n}", recorder.Body.String(), "Expected Must have body, but got %s", recorder.Body.String())
}

func TestPutNextServerBadJson(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb/next_server/1.1.1.1")
	req, err := http.NewRequest("PUT", url, strings.NewReader("greg"))
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 400, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"invalid character 'g' looking for beginning of value\"\n}", recorder.Body.String(), "Expected invalid character 'g' looking for beginning of value, but got %s", recorder.Body.String())
}

func TestPutNextServerMissingSubnet(t *testing.T) {
	fe, handler := get_frontend()

	recorder := httptest.NewRecorder()
	url := base_url("/subnets/jeb/next_server/1.1.1.1")
	req, err := http.NewRequest("PUT", url, strings.NewReader("{}"))
	assert.Nil(t, err)
	req.SetBasicAuth("fred", "rules")
	req.Header.Add("Content-Type", "application/json")

	// Clear all subnets
	for k := range fe.DhcpInfo.Subnets {
		delete(fe.DhcpInfo.Subnets, k)
	}

	handler.ServeHTTP(recorder, req)

	assert.Equal(t, 404, recorder.Code)
	assert.Equal(t, "{\n  \"Error\": \"Not Found\"\n}", recorder.Body.String(), "Expected Not Found, but got %s", recorder.Body.String())
}
