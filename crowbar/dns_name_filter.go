package main

import (
	"encoding/json"
	"fmt"

	crowbar "github.com/VictorLowther/crowbar-api"
)

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		dnsNameFilters, err := crowbar.DnsNameFilters()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(dnsNameFilters))
		for i := range dnsNameFilters {
			res[i] = dnsNameFilters[i]
		}
		return res, nil
	}
	matcher := func(sample string) (string, error) {
		obj := &crowbar.DnsNameFilter{}
		err := json.Unmarshal([]byte(sample), obj)
		if err != nil {
			return "", fmt.Errorf("Error unmarshalling dnsnamefilter\nError: %v\n", err.Error())
		}
		objs, err := obj.Match()
		if err != nil {
			return "", fmt.Errorf("Error fetching matches for %v", sample)
		}
		return prettyJSON(objs), nil
	}
	maker := func() crowbar.Crudder { return &crowbar.DnsNameFilter{} }
	singularName := "dnsnamefilter"
	app.AddCommand(makeCommandTree(singularName, lister, matcher, maker))
}
