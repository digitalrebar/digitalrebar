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
	"regexp"
	"strings"
	"sync"
	"time"

	"github.com/hashicorp/consul/api"
)

// Global lock for the default registry.
var lock sync.RWMutex

// Common errors.
var (
	ErrServiceNotFound = errors.New("service tag not found")
)

/* Example of DefaultRegistry without Consul
var ServiceRegistry = &DefaultRegistry{
	Map: map[string][]string{
		"dhcp": []string{
			"192.168.99.100:6755",
		},
		"dns": []string{
			"192.168.99.100:6754",
		},
		"provisioner": []string{
			"192.168.99.100:8092",
		},
		"rebarapi": []string{
			"192.168.99.100:3000",
		},
	},
	Matcher: map[string]*regexp.Regexp{
		"dhcp":        regexp.MustCompile("^dhcp/(.*)"),
		"dns":         regexp.MustCompile("^dns/(.*)"),
		"provisioner": regexp.MustCompile("^provisioner/(.*)"),
		"rebarapi":    regexp.MustCompile("^rebarapi/(.*)"),
	},
	Default: "rebarapi",
}
*/

var ServiceRegistry = &ConsulRegistry{}

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
	Default string
}

func stringInSlice(a string, list []string) bool {
	for _, b := range list {
		if b == a {
			return true
		}
	}
	return false
}

type ConsulRegistry struct {
	DefaultRegistry
}

func (c *ConsulRegistry) WatchConsul() {
	c.Map = make(map[string][]string, 0)
	c.Matcher = make(map[string]*regexp.Regexp, 0)

	client, err := api.NewClient(api.DefaultConfig())
	if err != nil {
		log.Fatal("Failed to attach to consul agent: ", err)
	}

	kv := client.KV()
	catalog := client.Catalog()
	q := api.QueryOptions{
		WaitIndex: 0,
		WaitTime:  time.Second * 10,
	}

	knownServices := make(map[string]*api.CatalogService)
	for {
		services, meta, err := catalog.Services(&q)
		if err != nil {
			log.Fatal("Failed to get service catalog from consul agent: ", err)
		}
		wantedServices := make(map[string]*api.CatalogService)
		toRemoveServices := make(map[string]*api.CatalogService)
		untouchedServices := make(map[string]*api.CatalogService)
		for svcName := range services {
			svcCatalog, _, err := catalog.Service(svcName, "", nil)
			if err != nil {
				log.Fatal("Failed to get service entry from consul agent: ", err)
			}
			if len(svcCatalog) == 0 {
				continue
			}
			svc := svcCatalog[0]
			if !stringInSlice("revproxy", svc.ServiceTags) {
				continue
			}
			if stringInSlice("revproxy-default", svc.ServiceTags) {
				c.Default = svcName
			}

			// Bucketize the services we want to forward
			if knownSvc, ok := knownServices[svcName]; ok {
				if knownSvc.Address == svc.Address &&
					knownSvc.ServiceAddress == svc.ServiceAddress &&
					knownSvc.ServicePort == svc.ServicePort {
					// Nothing changed, it goes in the untouched bucket.
					untouchedServices[svcName] = knownSvc
				} else {
					// Something changed, it goes in the toRemove and
					// wanted buckets
					log.Printf("%s has changed config, will update forwarding rules", svcName)
					log.Printf("%s: ServiceAddress %v => %v", svcName, knownSvc.ServiceAddress, svc.ServiceAddress)
					log.Printf("%s: ServicePort %v => %v", svcName, knownSvc.ServicePort, svc.ServicePort)
					log.Printf("%s: Address %v => %v", svcName, knownSvc.Address, svc.Address)
					toRemoveServices[svcName] = knownSvc
					wantedServices[svcName] = svc
				}
			} else {
				log.Printf("%s is new, will add to rules", svcName)
				log.Printf("%s: ServiceAddress %v", svcName, svc.ServiceAddress)
				log.Printf("%s: ServicePort %v", svcName, svc.ServicePort)
				log.Printf("%s: Address %v", svcName, svc.Address)
				// New service, it goes in the wanted bucket
				wantedServices[svcName] = svc
			}
		}
		// Any known services that are not in the wanted or untouched bucket
		// need to be removed.
		for svcName, svc := range knownServices {
			if _, ok := wantedServices[svcName]; ok {
				continue
			}
			if _, ok := untouchedServices[svcName]; ok {
				continue
			}
			log.Printf("%s has gone away, will delete forwarding rules", svcName)
			toRemoveServices[svcName] = svc
		}
		// Delete services we no longer care about
		for svcTag, svc := range toRemoveServices {
			// Whack service registration iff the service was removed.
			svcAddr := svc.ServiceAddress
			if svcAddr == "" {
				svcAddr = svc.Address
			}
			svcPort := fmt.Sprintf("%d", svc.ServicePort)

			c.Delete(svcTag, svcAddr+":"+svcPort)
		}
		// Add new services we do care about
		for svcTag, svc := range wantedServices {
			svcAddr := svc.ServiceAddress
			if svcAddr == "" {
				svcAddr = svc.Address
			}
			svcMatcher := "jjk"
			kp, _, err := kv.Get("digitalrebar/public/revproxy/"+svcTag+"/matcher", nil)
			if err != nil {
				log.Printf("kv lookup err: %v", err)
			} else {
				if kp.Value != nil {
					svcMatcher = string(kp.Value[:])
				} else {
					svcMatcher = "^" + svcTag + "/(.*)"
				}
			}
			svcPort := fmt.Sprintf("%d", svc.ServicePort)

			c.Add(svcTag, svcMatcher, svcAddr+":"+svcPort)
			untouchedServices[svcTag] = svc
		}
		knownServices = untouchedServices
		q.WaitIndex = meta.LastIndex
	}

}

// extractTag lookup the target path and extract the tag
// It updates the target Path trimming part to get to tag
func (r *DefaultRegistry) ExtractTag(target *url.URL) (tag string, err error) {
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
		_, err := r.LookupTag(r.Default)
		if err != nil {
			return "", fmt.Errorf("Invalid path")
		} else {
			return r.Default, nil
		}
	}

	return tag, nil
}

// Lookup return the endpoint list for the given service
func (r *DefaultRegistry) LookupTag(tag string) ([]string, error) {
	lock.RLock()
	targets, ok := r.Map[tag]
	lock.RUnlock()
	if !ok {
		return nil, ErrServiceNotFound
	}
	return targets, nil
}

// Failure marks the given endpoint for service as failed.
func (r *DefaultRegistry) Failure(tag, endpoint string, err error) {
	// Would be used to remove an endpoint from the rotation, log the failure, etc.
	log.Printf("Error accessing %s (%s): %s", tag, endpoint, err)
}

// Add adds the given endpoit for the service
func (r *DefaultRegistry) Add(tag, matcher, endpoint string) {
	lock.Lock()
	defer lock.Unlock()

	service, ok := r.Map[tag]
	if !ok {
		service = []string{}
		r.Map[tag] = service
		r.Matcher[tag] = regexp.MustCompile(strings.TrimSpace(matcher))
	}
	r.Map[tag] = append(service, endpoint)
}

// Delete removes the given endpoit for the service name/version.
func (r *DefaultRegistry) Delete(tag, endpoint string) {
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
