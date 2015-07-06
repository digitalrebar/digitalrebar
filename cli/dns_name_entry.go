package main

import (
	"encoding/json"
	"fmt"

	crowbar "github.com/VictorLowther/crowbar-api"
)

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		dnsNameEntrys, err := crowbar.DnsNameEntrys()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(dnsNameEntrys))
		for i := range dnsNameEntrys {
			res[i] = dnsNameEntrys[i]
		}
		return res, nil
	}
	matcher := func(sample string) (string, error) {
		obj := &crowbar.DnsNameEntry{}
		err := json.Unmarshal([]byte(sample), obj)
		if err != nil {
			return "", fmt.Errorf("Error unmarshalling dnsnameentry\nError: %v\n", err.Error())
		}
		objs, err := obj.Match()
		if err != nil {
			return "", fmt.Errorf("Error fetching matches for %v", sample)
		}
		return prettyJSON(objs), nil
	}
	maker := func() crowbar.Crudder { return &crowbar.DnsNameEntry{} }
	singularName := "dnsnameentry"
	app.AddCommand(makeCommandTree(singularName, lister, matcher, maker))
}
