package main

import (
	"encoding/json"
	"fmt"

	crowbar "github.com/VictorLowther/crowbar-api"
)

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		users, err := crowbar.Users()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(users))
		for i := range users {
			res[i] = users[i]
		}
		return res, nil
	}
	matcher := func(sample string) (string, error) {
		obj := &crowbar.User{}
		err := json.Unmarshal([]byte(sample), obj)
		if err != nil {
			return "", fmt.Errorf("Error unmarshalling user\nError: %v\n", err.Error())
		}
		objs, err := obj.Match()
		if err != nil {
			return "", fmt.Errorf("Error fetching matches for %v", sample)
		}
		return prettyJSON(objs), nil
	}
	maker := func() crowbar.Crudder { return &crowbar.User{} }
	singularName := "user"
	app.AddCommand(makeCommandTree(singularName, lister, matcher, maker))
}
