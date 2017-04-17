package service

import (
	"fmt"
	"net"
	"os"
	"strings"
	"time"

	"github.com/hashicorp/consul/api"
)

// Register registers a service with Consul.  It automatically handles setting up forwarder and
// revproxy registration.
func Register(client *api.Client, svc *api.AgentServiceRegistration, allowExternal bool) error {
	revproxy := false
	revproxyFormat := "^%s/(.*)"
	for _, t := range svc.Tags {
		if t == "revproxy" {
			revproxy = true
		} else if t == "revproxyapi" {
			revproxy = true
			revproxyFormat = "^%s/(api/.*)"
		}
	}
	if revproxy && allowExternal {
		return fmt.Errorf("service.Register %s: cannot use revproxy and advertise an external address at the same time.", svc.Name)
	}
	if revproxy {
		rpName := strings.TrimSuffix(svc.Name, "-service")
		// Match what we do with the bash code
		rpName = strings.TrimSuffix(rpName, "-mgmt")
		_, err := client.KV().Put(&api.KVPair{
			Key:   fmt.Sprintf("digitalrebar/public/revproxy/%s/matcher", svc.Name),
			Value: []byte(fmt.Sprintf(revproxyFormat, rpName)),
		}, nil)
		if err != nil {
			return err
		}
	}
	if extIP := os.Getenv("EXTERNAL_IP"); extIP != "" {
		ip, _, err := net.ParseCIDR(extIP)
		if err != nil {
			return fmt.Errorf("service.Register: EXTERNAL_IP not CIDR: %v", err)
		}
		svc.Address = ip.String()
	}
	return client.Agent().ServiceRegister(svc)
}

// GetService is a thin wrapper around the consul API Service function
func GetService(client *api.Client, name, tag string) ([]*api.CatalogService, error) {
	catalog := client.Catalog()
	q := api.QueryOptions{
		WaitIndex: 0,
		WaitTime:  time.Second * 10,
	}
	cs, _, err := catalog.Service(name, tag, &q)
	return cs, err
}

// WaitService works like GetService, except it will wait until the
// service shows up.
func WaitService(client *api.Client, name, tag string) ([]*api.CatalogService, error) {
	catalog := client.Catalog()
	q := api.QueryOptions{
		WaitIndex: 0,
		WaitTime:  time.Second * 10,
	}
	for true {
		cs, qm, err := catalog.Service(name, tag, &q)
		if err != nil {
			return nil, err
		}
		if len(cs) > 0 {
			return cs, nil
		}
		q.WaitIndex = qm.LastIndex
	}
	return []*api.CatalogService{}, nil
}

// Look for a Rebar registered service
// It looks for service names in the following order:
//    - internal-name-service
//    - name-service
//    - name
func Find(client *api.Client, name, tag string) ([]*api.CatalogService, error) {
	catalog := client.Catalog()
	q := api.QueryOptions{
		WaitIndex: 0,
		WaitTime:  time.Second * 10,
	}
	names := []string{
		"internal-" + name + "-service",
		name + "-service",
		name,
	}
	for i := range names {
		s, _, err := catalog.Service(names[i], tag, &q)
		if err != nil {
			return nil, err
		}
		if len(s) > 0 {
			return s, nil
		}
	}
	return []*api.CatalogService{}, nil
}

// Address extracts the address/port from a returned CatalogService
func Address(s *api.CatalogService) (addr string, port int) {
	port = s.ServicePort
	addr = s.Address
	if s.ServiceAddress != "" {
		addr = s.ServiceAddress
	}
	return
}
