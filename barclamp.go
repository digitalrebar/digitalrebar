package crowbar

import (
	"errors"
	"log"
	"strconv"
	//	"encoding/json"
	//	"os"
	//	"path/filepath"
	//	yaml "gopkg.in/yaml.v2"
	//	"io/ioutil"
)

type RawBarclamp struct {
	Name        string `json:"name,omitempty" yaml:"name,omitempty"`
	Description string `json:"description,omitempty" yaml:"description,omitempty"`
	Parent      string `json:"parent,omitempty" yaml:"parent,omitempty`
	Display     string `json:"display,omitempty" yaml:"display,omitempty"`
	Version     string `json:"version,omitempty" yaml:"version,omitempty"`
	SourceURL   string `json:"source_url,omitempty" yaml:"source_url,omitempty"`
	SourcePath  string `json:"source_path,omitempty" yaml:"source_path,omitempty"`
	License     string `json:"license,omitempty" yaml:"license,omitempty"`
	LicenseURL  string `json:"license_url,omitempty" yaml:"license_url,omitempty"`
}

type RawRole struct {
	Role
	Attribs []*Attrib `json:"attribs,omitempty" yaml:"attribs,omitempty"`
	Flags   []string  `json:"flags,omitempty" yaml:flags,omitempty"`
}

type RawCfgData struct {
	Barclamp   *RawBarclamp       `json:"barclamp,omitempty" yaml:"barclamp,omitempty"`
	Roles      []*RawRole         `json:"roles,omitempty" yaml:"roles,omitempty"`
	Attribs    []*Attrib          `json:"attribs,omitempty" yaml:"attribs,omitempty"`
	Jigs       []*Jig             `json:"jigs,omitempty" yaml:"jigs,omitempty"`
	Hammers    []*AvailableHammer `json:"hammers,omitempty" yaml:"hammers,omitempty"`
	OsSupport  []string           `json:"os_support,omitempty" yaml:"os_support,omitempty"`
	ExtraFiles []string           `json:"extra_files,omitempty" yaml:"extra_files,omitempty"`
	//	Crowbar    interface{}        `json:"crowbar,omitempty" yaml:"crowbar,omitempty"`
	//	Debs       interface{}        `json:"debs,omitempty" yaml:"debs,omitempty"`
	//	Rpms       interface{}        `json:"rpms,omitempty" yaml:"rpms,omitempty"`
	//	Gems       interface{}        `json:"gems,omitempty" yaml:"gems,omitempty"`
}

type bcImport struct {
	Value *RawCfgData `json:"value,omitempty"`
}

type Barclamp struct {
	ID          int64       `json:"id,omitempty"`
	Name        string      `json:"name,omitempty"`
	Description string      `json:"description,omitempty"`
	ParentID    int64       `json:"barclamp_id,omitempty"`
	Version     string      `json:"version,omitempty"`
	SourceURL   string      `json:"source_url,omitempty"`
	SourcePath  string      `json:"source_path,omitempty"`
	CfgData     *RawCfgData `json:"cfg_data,omitempty"`
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

func (o *Barclamp) Match() (res []*Barclamp, err error) {
	res = make([]*Barclamp, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

/*  This does not actually work right now due to YAML unmarshalling not
    being compatible with remarshalling into JSON without fully specifying everything.

// Yes, this is in dire need of cleanup
func importBarclamp(path string) (res *Barclamp, err error) {
	config, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer config.Close()
	buf, err := ioutil.ReadAll(config)
	rawBarclamp := &RawCfgData{}
	if err := yaml.Unmarshal(buf, rawBarclamp); err != nil {
		return nil, err
	}
	valuer := make(map[string]*RawCfgData)
	valuer["value"] = rawBarclamp
	// outBuf, err := json.Marshal(valuer)
	_, err = json.Marshal(valuer)
	if err != nil {
		log.Panicf("raw_bc: %#v\n",rawBarclamp.Roles)
		return nil, err
	}
	//inBuf, err := session.request("POST", "barclamps", outBuf)
	//if err != nil {
	//	return nil, err
	//}
	res = &Barclamp{}
	//if err = json.Unmarshal(inBuf, res); err != nil {
	//	return nil, err
	//}
	return res, nil
}

func BarclampImport(path string) (res []*Barclamp, err error) {
	path, err = filepath.Abs(path)
	if err != nil {
		return nil, err
	}
	if filepath.Base(path) != "crowbar.yml" {
		path = filepath.Join(path, "crowbar.yml")
	}
	res = []*Barclamp{}
	barclamp, err := importBarclamp(path)
	if err != nil {
		return nil, err
	}
	res = append(res, barclamp)
	children, err := filepath.Glob(filepath.Join(filepath.Dir(path), "barclamps", "*.yml"))
	if err != nil || len(children) == 0 {
		return res, nil
	}
	for _, child := range children {
		barclamp, err = importBarclamp(child)
		if err != nil {
			return nil, err
		}
		res = append(res, barclamp)
	}
	return res, nil
}
*/

func Barclamps() (res []*Barclamp, err error) {
	res = make([]*Barclamp, 0)
	return res, session.list(&res, "barclamps")
}
