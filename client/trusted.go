// Package client is where you can easily create Rebar clients.
// For now we only handle creating trusted clients.
package client

import (
	"fmt"
	"log"
	"time"

	"github.com/digitalrebar/go-common/service"
	"github.com/digitalrebar/rebar-api/api"
	consul "github.com/hashicorp/consul/api"
)

// Consul gets a local Consul client using the default config.
// If wait is true, it will wait until it can fetch the client.
func Consul(wait bool) (*consul.Client, error) {
	for {
		cClient, err := consul.NewClient(consul.DefaultConfig())
		if err == nil {
			return cClient, err
		}
		if !wait {
			return nil, fmt.Errorf("RebarClient: Error getting local Consul client: %v", err)
		}
		time.Sleep(10 * time.Second)
	}
}

// Trusted creates a fully preconfigured trusted Rebar API client based on information in Consul.
// If wait is true, this function will wait until the Rebar API is registered in Consul
func Trusted(user string, wait bool) (*api.Client, error) {
	cClient, err := Consul(wait)
	if err != nil {
		return nil, err
	}
	for {
		apiService, err := service.Find(cClient, "rebar-api", "")
		if err != nil {
			return nil, fmt.Errorf("RebarClient: Error talking to local Consul: %v", err)
		}
		if len(apiService) == 0 {
			if wait {
				log.Println("Rebar API service not registered yet")
				time.Sleep(10 * time.Second)
				continue
			}
			return nil, fmt.Errorf("RebarClient: Rebar API not registered with Consul")
		}
		apiAddr, apiPort := service.Address(apiService[0])
		return api.TrustedSession(fmt.Sprintf("https://%s:%d", apiAddr, apiPort), user)
	}
}
