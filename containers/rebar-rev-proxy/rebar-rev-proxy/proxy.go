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

package main

import (
	"crypto/tls"
	"errors"
	"fmt"
	"log"
	"math/rand"
	"net"
	"net/http"
	"net/http/httputil"
	"strings"
	"time"
)

// Common errors
var (
	ErrInvalidService = errors.New("invalid service/version")
)

// loadBalance is a basic loadBalancer which randomly
// tries to connect to one of the endpoints and try again
// in case of failure.
func loadBalance(network, endpointTag string, reg Registry) (net.Conn, error) {
	endpoints, err := reg.LookupTag(endpointTag)
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
			reg.Failure(endpointTag, endpoint, err)
			// Failure: remove the endpoint from the current list and try again.
			endpoints = append(endpoints[:i], endpoints[i+1:]...)
			continue
		}
		// Success: return the connection.
		return conn, nil
	}
	// No available endpoint.
	return nil, fmt.Errorf("No endpoint available for %s", endpointTag)
}

// NewMultipleHostReverseProxy creates a reverse proxy handler
// that will randomly select a host from the passed `targets`
func NewMultipleHostReverseProxy(reg Registry, tlsConfig *tls.Config) http.HandlerFunc {
	transport := &http.Transport{
		Proxy: http.ProxyFromEnvironment,
		Dial: func(network, addr string) (net.Conn, error) {
			addr = strings.Split(addr, ":")[0]
			return loadBalance(network, addr, reg)
		},
		TLSHandshakeTimeout: 10 * time.Second,
		TLSClientConfig:     tlsConfig,
	}
	return func(w http.ResponseWriter, req *http.Request) {
		log.Printf("requested  url = %v\n", req.URL)
		tag, err := reg.ExtractTag(req.URL)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		(&httputil.ReverseProxy{
			Director: func(req *http.Request) {
				req.URL.Scheme = "https"
				req.URL.Host = tag
				log.Printf("translated url = %v\n", req.URL)
			},
			Transport: transport,
		}).ServeHTTP(w, req)
	}
}
