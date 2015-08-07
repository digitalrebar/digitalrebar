package crowbar

import (
	"errors"
	"log"
	"strconv"
)

type Barclamp struct {
	ID          int64  `json:"id"`
	Name        string `json:"name"`
	Description string `json:"description"`
	ParentID    int64  `json:"barclamp_id"`
	Version     string `json:"version"`
	SourceURL   string `json:"source_url"`
	SourcePath  string `json:"source_path"`
	// CfgData     *RawCfgData `json:"cfg_data"`
	CreatedAt string `json:"created_at"`
	UpdatedAt string `json:"updated_at"`
	lastJson  []byte
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

func (o *Barclamp) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *Barclamp) lastJSON() []byte {
	return o.lastJson
}

func Barclamps() (res []*Barclamp, err error) {
	res = make([]*Barclamp, 0)
	return res, session.list(&res, "barclamps")
}
