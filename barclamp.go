package crowbar

import (
	"errors"
	"log"
	"strconv"
)

type RawBarclamp struct {
	Name        string `json:"name"`
	Description string `json:"description,omitempty"`
	Parent      string `json:"parent,omitempty"`
	Display     string `json:"display,omitempty"`
	Version     string `json:"version,omitempty"`
	SourceURL   string `json:"source_url,omitempty"`
	SourcePath  string `json:"source_path,omitempty"`
	License     string `json:"license,omitempty"`
	LicenseURL  string `json:"license_url,omitempty"`
}

type RawRole struct {
	Role
	Attribs []*Attrib `json:"attribs,omitempty"`
}

type RawCfgData struct {
	Barclamp RawBarclamp        `json:"barclamp,omitempty"`
	Roles    []*RawRole         `json:"roles,omitempty"`
	Attribs  []*Attrib          `json:"attribs,omitempty"`
	Jigs     []*Jig             `json:"jigs,omitempty"`
	Hammers  []*AvailableHammer `json:"hammers,omitempty"`
}

type Barclamp struct {
	ID          int64       `json:"id,omitempty"`
	Name        string      `json:"name,omitempty"`
	Description string      `json:"description,omitempty"`
	ParentID    int64       `json:"barclamp_id,omitempty"`
	Version     interface{} `json:"version,omitempty"`
	SourceURL   string      `json:"source_url,omitempty"`
	SourcePath  string      `json:"source_path,omitempty"`
	CfgData     RawCfgData  `json:"cfg_data,omitempty"`
	CreatedAt   string      `json:"created_at,omitempty"`
	UpdatedAt   string      `json:"updated_at,omitempty"`
}

func (o *Barclamp) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Barclamp has no ID or name")
		return ""
	}
}

func (o *Barclamp) SetId(s string) error {
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

func (o *Barclamp) ApiName() string {
	return "barclamps"
}

func Barclamps() (res []*Barclamp, err error) {
	res = make([]*Barclamp, 0)
	return res, session.list(&res, "barclamps")
}
