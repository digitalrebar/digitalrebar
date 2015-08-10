package datatypes

import "strconv"
import "github.com/guregu/null"

type User struct {
	ID       int64       `json:"id"`
	Name     string      `json:"username"`
	Email    string      `json:"email"`
	Admin    bool        `json:"is_admin"`
	Locked   bool        `json:"locked"`
	Password null.String `json:"password"`
}

func (o *User) Id() (string, error) {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10), nil
	} else if o.Name != "" {
		return o.Name, nil
	} else {
		return "", IDNotSet
	}
}

func (o *User) SetId(s string) error {
	if o.ID != 0 || o.Name != "" {
		return SetIDErr
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
