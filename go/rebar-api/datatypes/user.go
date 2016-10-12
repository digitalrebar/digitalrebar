package datatypes

import "path"

type User struct {
	SimpleID
	Name            string `json:"username"`
	Email           string `json:"email"`
	Admin           bool   `json:"is_admin"`
	TenantID        int64  `json:"tenant_id,omitempty"`
	CurrentTenantID int64  `json:"current_tenant_id,omitempty"`
	Locked          bool   `json:"locked"`
}

// Id returns this attrib's ID or Name as a string.
// The REST API allows them to be used interchangeably.
func (o *User) Id() (string, error) {
	if o.Name != "" {
		return o.Name, nil
	}
	return o.SimpleID.Id()
}

// SetId sets either the ID or the Name field, depending on whether
// the passed-in string can be parsed as an int64 or not.
func (o *User) SetId(s string) error {
	err := o.SimpleID.SetId(s)
	if err == nil {
		return nil
	} else if err == SetIDErr {
		return err
	}
	o.Name = s
	return nil
}

func (o *User) ApiName() string {
	return "users"
}

func (o *User) ApiPath() string {
	return path.Join(API_PATH, o.ApiName())
}
