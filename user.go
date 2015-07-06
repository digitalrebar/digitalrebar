package crowbar

import (
	"errors"
	"log"
	"strconv"
)

type User struct {
	ID        int64  `json:"id,omitempty"`
	Name      string `json:"username,omitempty"`
	Email     string `json:"email,omitempty"`
	Admin     bool   `json:"is_admin,omitempty"`
	Locked    bool   `json:"locked,omitempty"`
	Password  string `json:"password,omitempty"`
	CreatedAt string `json:"created_at,omitempty"`
	UpdatedAt string `json:"updated_at,omitempty"`
}

func (o *User) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("User has no ID or name")
		return ""
	}
}

func (o *User) SetId(s string) error {
	if o.ID != 0 || o.Name != "" {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		o.Name = s
	}
	return nil
}

func (o *User) ApiName() string {
	return "users"
}

func (o *User) Match() (res []*User, err error) {
	res = make([]*User, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

func Users() (res []*User, err error) {
	res = make([]*User, 0)
	return res, session.list(&res, "users")
}
