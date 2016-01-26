package datatypes

import (
	"errors"

	"github.com/guregu/null"
)

// Barclamp tracks the Barclamps that Rebar can work with.
type Barclamp struct {
	NameID
	Description string      `json:"description"`
	ParentID    null.Int    `json:"barclamp_id"`
	Version     null.String `json:"version"`
	SourceURL   null.String `json:"source_url"`
	SourcePath  null.String `json:"source_path"`
	License     null.String `json:"license"`
	LicenseURL  null.String `json:"license_url`
	// CfgData     *RawCfgData `json:"cfg_data"`
}

type jigImport struct {
	Name        string `json:"name",yaml:"name"`
	Class       string `json:"class",yaml:"class"`
	Description string `json:"description",yaml:"description"`
}

type attribImport struct {
	Name        string      `json:"name",yaml:"name"`
	Description string      `json:"description",yaml:"description"`
	Map         string      `json:"map",yaml:"map"`
	Default     interface{} `json:"default",yaml:"default"`
	Schema      interface{} `json:"schema",yaml:"schema"`
	SchemaType  string      `json:"schematype",yaml:"schematype"`
}

func fixUp(i interface{}) (interface{}, error) {
	switch t := i.(type) {
	case map[interface{}]interface{}:
		res := map[string]interface{}{}
		for k, v := range t {
			if str, ok := k.(string); !ok {
				return nil, errors.New("Cannot fixup map!")
			} else {
				v, err := fixUp(v)
				if err != nil {
					return nil, err
				}
				res[str] = v
			}
		}
		return res, nil
	case []interface{}:
		res := make([]interface{}, len(t))
		for i := range t {
			v, err := fixUp(t[i])
			if err != nil {
				return nil, errors.New("Cannot fixup slice!")
			}
			res[i] = v
		}
		return res, nil
	default:
		return i, nil
	}
}

type roleImport struct {
	Name         string         `json:"name",yaml:"name"`
	Jig          string         `json:"jig",yaml:"jig"`
	Description  string         `json:"description",yaml:"description"`
	Requires     []string       `json:"requires",yaml:"requires"`
	Conflicts    []string       `json:"conflicts",yaml:"conflicts"`
	Provides     []string       `json:"provides",yaml:"provides"`
	Flags        []string       `json:"flags",yaml:"flags"`
	Attribs      []attribImport `json:"attribs",yaml:"attribs"`
	WantsAttribs string         `json:"wants_attribs",yaml:"wants_attribs"`
}

type hammerImport struct {
	Name        string `json:"name",yaml:"name"`
	Type        string `json:"type",yaml:"type"`
	Description string `json:"description",yaml:"description"`
	Priority    int64  `json:"priority",yaml:"priority"`
}

type barclampImport struct {
	Name        string `json:"name",yaml:"name"`
	Parent      string `json:"parent,omitempty",yaml:"parent,omitempty"`
	Display     string `json:"display",yaml:"display"`
	Description string `json:"description",yaml:"description"`
	Version     string `json:"version",yaml:"version"`
	SourceURL   string `json:"source_url",yaml:"source_url"`
	SourcePath  string `json:"source_path",yaml:"source_path"`
	License     string `json:"license",yaml:"license"`
	LicenseURL  string `json:"license_url",yaml:"license_url"`
}

type BarclampImport struct {
	Barclamp barclampImport `json:"barclamp",yaml:"barclamp"`
	Rebar  struct {
		Layout float64 `json:"layout",yaml:"layout"`
	} `json:"rebar",yaml:"rebar"`
	Jigs       []jigImport    `json:"jigs",yaml:"jigs"`
	Roles      []roleImport   `json:"roles",yaml:"roles"`
	Attribs    []attribImport `json:"attribs",yaml:"attribs"`
	Hammers    []hammerImport `json:"hammers",yaml:"hammers"`
	OsSupport  []string       `json:"os_support",yaml:"os_support"`
	ExtraFiles []string       `json:"extra_files",yaml:"extra_files"`
	Debs       interface{}    `json:"debs",yaml:"debs"`
	Rpms       interface{}    `json:"rpms",yaml:"rpms"`
	Gems       interface{}    `json:"gems",yaml:"gems"`
}

func (o *BarclampImport) FixupYAMLImport() error {
	for r := range o.Roles {
		for i := range o.Roles[r].Attribs {
			def, err := fixUp(o.Roles[r].Attribs[i].Default)
			if err != nil {
				return err
			}
			sch, err := fixUp(o.Roles[r].Attribs[i].Schema)
			if err != nil {
				return err
			}
			o.Roles[r].Attribs[i].Default = def
			o.Roles[r].Attribs[i].Schema = sch
		}
	}
	for i := range o.Attribs {
		def, err := fixUp(o.Attribs[i].Default)
		if err != nil {
			return err
		}
		sch, err := fixUp(o.Attribs[i].Schema)
		if err != nil {
			return err
		}
		o.Attribs[i].Default = def
		o.Attribs[i].Schema = sch
	}
	var err error
	d, err := fixUp(o.Debs)
	if err != nil {
		return err
	}
	r, err := fixUp(o.Rpms)
	if err != nil {
		return err
	}
	g, err := fixUp(o.Gems)
	if err != nil {
		return err
	}
	o.Debs = d
	o.Rpms = r
	o.Gems = g
	return nil
}

func (o *Barclamp) ApiName() string {
	return "barclamps"
}
