package crowbar

import "github.com/VictorLowther/crowbar-api/datatypes"

type User struct {
	datatypes.User
	Timestamps
	apiHelper
}

func Users() (res []*User, err error) {
	res = make([]*User, 0)
	return res, List("users", &res)
}
