package crowbar

import (
	"log"
	"path"
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

func (o *Role) id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Role has no ID or name")
		return ""
	}
}

func (o *Role) url(parts ...string) string {
	return path.Join(append([]string{"roles", o.id()}, parts...)...)
}

func (o *Role) Get() error {
	return Get(o, o.url())
}

func (o *Role) Delete() error {
	return Delete(o.url())
}

func (o *Role) Create() error {
	return Post(o, "roles")
}

func (o *Role) Update() error {
	return Put(o, o.url())
}

func (o *Role) Attribs() (res []*Attrib, err error) {
	res = []*Attrib{}
	err = Get(res, o.url("attribs"))
	return res, err
}

func (o *Role) GetAttrib(a *Attrib) error {
	return Get(a, o.url(a.url()))
}

func (o *Role) SetAttrib(a *Attrib) error {
	return Put(a, o.url(a.url()))
}

func Roles() (res []*Role, err error) {
	res = []*Role{}
	err = Get(res, "roles")
	return res, err
}
