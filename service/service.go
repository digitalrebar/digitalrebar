package service

import (
	"time"

	"github.com/hashicorp/consul/api"
)

func GetService(client *api.Client, name, tag string) ([]*api.CatalogService, error) {
	catalog := client.Catalog()
	q := api.QueryOptions{
		WaitIndex: 0,
		WaitTime:  time.Second * 10,
	}
	cs, _, err := catalog.Service(name, tag, &q)
	return cs, err
}

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
