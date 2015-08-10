package datatypes

import "github.com/guregu/null"

type Barclamp struct {
	NameID
	Description string      `json:"description"`
	ParentID    null.Int    `json:"barclamp_id"`
	Version     null.Int    `json:"version"`
	SourceURL   null.String `json:"source_url"`
	SourcePath  null.String `json:"source_path"`
	// CfgData     *RawCfgData `json:"cfg_data"`
}

func (o *Barclamp) ApiName() string {
	return "barclamps"
}
