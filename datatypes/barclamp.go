package datatypes

type Barclamp struct {
	NameID
	Description string `json:"description"`
	ParentID    int64  `json:"barclamp_id"`
	Version     string `json:"version"`
	SourceURL   string `json:"source_url"`
	SourcePath  string `json:"source_path"`
	// CfgData     *RawCfgData `json:"cfg_data"`
}

func (o *Barclamp) ApiName() string {
	return "barclamps"
}
