// Package client is where you can easily create Rebar clients.
// For now we only handle creating trusted clients.
package client

import (
	"fmt"
	"time"

	consul "github.com/hashicorp/consul/api"
)

// Consul gets a local Consul client using the default config.
// If wait is true, it will wait until it can fetch the client.
func Consul(wait bool) (*consul.Client, error) {
	for {
		cClient, err := consul.NewClient(consul.DefaultConfig())
		if err == nil {
			if _, err = cClient.Agent().Self(); err == nil {
				return cClient, nil
			}
		}
		if !wait {
			return nil, fmt.Errorf("RebarClient: Error getting local Consul client: %v", err)
		}
		time.Sleep(10 * time.Second)
	}
}
