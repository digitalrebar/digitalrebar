package datatypes

import "path"

type Capability struct {
	NameID
	Description string `json:"description"`
	Source      string `json:"source"`
}

func (o *Capability) ApiName() string {
	return "capabilities"
}

func (o *Capability) ApiPath() string {
	return path.Join(API_PATH, o.ApiName())
}
