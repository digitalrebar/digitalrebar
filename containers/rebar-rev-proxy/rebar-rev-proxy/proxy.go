/*
The MIT License (MIT)

Copyright (c) 2015 Guillaume J. Charmes

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

Highly modified, but from: https://github.com/creack/goproxy
*/

// ExtractNameVersion and LoadBalance can be overridden in order to customize
// the behavior.
package main

import (
	"crypto/tls"
	"errors"
	"fmt"
	"math/rand"
	"net"
	"net/http"
	"net/http/httputil"
	"net/url"
	"strings"
	"time"
)

// Common errors
var (
	ErrInvalidService = errors.New("invalid service/version")
)

// ExtractNameVersion is called to lookup the service name / version from
// the requested URL. It should update the URL's Path to reflect the target
// expectation.
var ExtractNameVersion = extractNameVersion

// LoadBalance is the default balancer which will use a random endpoint
// for the given service name/version.
var LoadBalance = loadBalance

// extractNameVersion lookup the target path and extract the name and version.
// It updates the target Path trimming version and name.
// Expected format: `/<name>/<version>/...`
func extractNameVersion(reg Registry, target *url.URL) (name, version string, err error) {
	path := target.Path
	if len(path) > 1 && path[0] == '/' {
		path = path[1:]
	}
	tmp := strings.Split(path, "/")
	if len(tmp) < 2 {
		_, err := reg.Lookup("default", "default")
		if err != nil {
			return "", "", fmt.Errorf("Invalid path")
		} else {
			return "default", "default", nil
		}

	}
	name, version = tmp[0], tmp[1]
	_, err = reg.Lookup(name, version)
	if err != nil {
		_, err = reg.Lookup("default", "default")
		if err != nil {
			return "", "", fmt.Errorf("Invalid path")
		}
		return "default", "default", nil
	}
	target.Path = "/" + strings.Join(tmp[2:], "/")
	return name, version, nil
}

// loadBalance is a basic loadBalancer which randomly
// tries to connect to one of the endpoints and try again
// in case of failure.
func loadBalance(network, serviceName, serviceVersion string, reg Registry) (net.Conn, error) {
	endpoints, err := reg.Lookup(serviceName, serviceVersion)
	if err != nil {
		return nil, err
	}
	for {
		// No more endpoint, stop
		if len(endpoints) == 0 {
			break
		}
		// Select a random endpoint
		i := rand.Int() % len(endpoints)
		endpoint := endpoints[i]

		// Try to connect
		conn, err := net.Dial(network, endpoint)
		if err != nil {
			reg.Failure(serviceName, serviceVersion, endpoint, err)
			// Failure: remove the endpoint from the current list and try again.
			endpoints = append(endpoints[:i], endpoints[i+1:]...)
			continue
		}
		// Success: return the connection.
		return conn, nil
	}
	// No available endpoint.
	return nil, fmt.Errorf("No endpoint available for %s/%s", serviceName, serviceVersion)
}

// NewMultipleHostReverseProxy creates a reverse proxy handler
// that will randomly select a host from the passed `targets`
func NewMultipleHostReverseProxy(reg Registry, tlsConfig *tls.Config) http.HandlerFunc {
	transport := &http.Transport{
		Proxy: http.ProxyFromEnvironment,
		Dial: func(network, addr string) (net.Conn, error) {
			addr = strings.Split(addr, ":")[0]
			tmp := strings.Split(addr, "/")
			if len(tmp) != 2 {
				return nil, ErrInvalidService
			}
			return LoadBalance(network, tmp[0], tmp[1], reg)
		},
		TLSHandshakeTimeout: 10 * time.Second,
		TLSClientConfig:     tlsConfig,
	}
	return func(w http.ResponseWriter, req *http.Request) {
		name, version, err := ExtractNameVersion(reg, req.URL)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		(&httputil.ReverseProxy{
			Director: func(req *http.Request) {
				req.URL.Scheme = "https"
				req.URL.Host = name + "/" + version
			},
			Transport: transport,
		}).ServeHTTP(w, req)
	}
}
