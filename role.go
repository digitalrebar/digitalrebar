package crowbar

import (
	"log"
	"strconv"
)

type Role struct {
	ID          int64         `json:"id,omitempty"`
	Name        string        `json:"name,omitempty"`
	Description string        `json:"description,omitempty"`
	BarclampID  int64         `json:"barclamp_id,omitempty"`
	JigName     string        `json:"jig_name,omitempty"`
	Abstract    bool          `json:"abstract,omitempty"`
	Bootstrap   bool          `json:"bootstrap,omitempty"`
	Cluster     bool          `json:"cluster,omitempty"`
	Destructive bool          `json:"destructive,omitempty"`
	Discovery   bool          `json:"discovery,omitempty"`
	Implicit    bool          `json:"implicit,omitempty"`
	Library     bool          `json:"library,omitempty"`
	Milestone   bool          `json:"milestone,omitempty"`
	Powersave   bool          `json:"powersave,omitempty"`
	Service     bool          `json:"service,omitempty"`
	Cohort      int           `json:"cohort,omitempty"`
	Conflicts   []interface{} `json:"conflicts,omitempty"`
	Provides    []interface{} `json:"provides,omitempty"`
	CreatedAt   string        `json:"created_at,omitempty"`
	UpdatedAt   string        `json:"updated_at,omitempty"`
}

func (o *Role) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Role has no ID or name")
		return ""
	}
}

func (o *Role) ApiName() string {
	return "roles"
}

func (o *Role) Attribs() (res []*Attrib, err error) {
	res = []*Attrib{}
	err = session.get(res, url(o, "attribs"))
	return res, err
}

func Roles(paths ...string) (res []*Role, err error) {
	res = make([]*Role, 0)
	return res, session.list(&res, append(paths, "roles")...)
}
