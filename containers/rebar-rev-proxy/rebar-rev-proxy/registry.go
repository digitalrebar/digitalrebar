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
	"errors"
	"fmt"
	"log"
	"net/url"
	//"strings"
	"regexp"
	"sync"
)

// Global lock for the default registry.
var lock sync.RWMutex

// Common errors.
var (
	ErrServiceNotFound = errors.New("service tag not found")
)

// Registry is an interface used to lookup the target host
// for a given service tag.
type Registry interface {
	Add(tag, matcher, endpoint string)          // Add an endpoint to our registry
	Delete(tag, endpoint string)                // Remove an endpoint to our registry
	Failure(tag, endpoint string, err error)    // Mark an endpoint as failed.
	LookupTag(tag string) ([]string, error)     // Return the endpoint list for the given service
	ExtractTag(target *url.URL) (string, error) // Return the tag to use to find the endpoints for given service
}

type DefaultRegistry struct {
	Map     map[string][]string
	Matcher map[string]*regexp.Regexp
}

type ConsulRegistry struct {
	DefaultRegistry
	// GREG: add routine to watch consul service catalog
}

// extractTag lookup the target path and extract the tag
// It updates the target Path trimming part to get to tag
func (r DefaultRegistry) ExtractTag(target *url.URL) (tag string, err error) {
	path := target.Path
	if len(path) > 1 && path[0] == '/' {
		path = path[1:]
	}

	found := false
	for itag, matcher := range r.Matcher {
		matches := matcher.FindStringSubmatch(path)
		if matches != nil {
			target.Path = "/" + matches[1]
			found = true
			tag = itag
			break
		}
	}

	if !found {
		_, err := r.LookupTag("rebarapi")
		if err != nil {
			return "", fmt.Errorf("Invalid path")
		} else {
			return "rebarapi", nil
		}
	}

	return tag, nil
}

// Lookup return the endpoint list for the given service
func (r DefaultRegistry) LookupTag(tag string) ([]string, error) {
	lock.RLock()
	targets, ok := r.Map[tag]
	lock.RUnlock()
	if !ok {
		return nil, ErrServiceNotFound
	}
	return targets, nil
}

// Failure marks the given endpoint for service as failed.
func (r DefaultRegistry) Failure(tag, endpoint string, err error) {
	// Would be used to remove an endpoint from the rotation, log the failure, etc.
	log.Printf("Error accessing %s (%s): %s", tag, endpoint, err)
}

// Add adds the given endpoit for the service
func (r DefaultRegistry) Add(tag, matcher, endpoint string) {
	lock.Lock()
	defer lock.Unlock()

	service, ok := r.Map[tag]
	if !ok {
		service = []string{}
		r.Map[tag] = service
		r.Matcher[tag] = regexp.MustCompile(matcher)
	}
	service = append(service, endpoint)
}

// Delete removes the given endpoit for the service name/version.
func (r DefaultRegistry) Delete(tag, endpoint string) {
	lock.Lock()
	defer lock.Unlock()

	service, ok := r.Map[tag]
	if !ok {
		return
	}
begin:
	for i, svc := range service {
		if svc == endpoint {
			copy(service[i:], service[i+1:])
			service[len(service)-1] = ""
			service = service[:len(service)-1]
			goto begin
		}
	}
}
