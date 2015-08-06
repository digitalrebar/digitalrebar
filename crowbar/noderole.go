package main

import (
	"encoding/json"
	"fmt"

	crowbar "github.com/VictorLowther/crowbar-api"
)

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		noderoles, err := crowbar.NodeRoles()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(noderoles))
		for i := range noderoles {
			res[i] = noderoles[i]
		}
		return res, nil
	}
	matcher := func(sample string) (string, error) {
		obj := &crowbar.NodeRole{}
		err := json.Unmarshal([]byte(sample), obj)
		if err != nil {
			return "", fmt.Errorf("Error unmarshalling noderole\nError: %v\n", err.Error())
		}
		objs, err := obj.Match()
		if err != nil {
			return "", fmt.Errorf("Error fetching matches for %v", sample)
		}
		return prettyJSON(objs), nil
	}
	maker := func() crowbar.Crudder { return &crowbar.NodeRole{} }
	singularName := "noderole"
	app.AddCommand(makeCommandTree(singularName, lister, matcher, maker))
}
