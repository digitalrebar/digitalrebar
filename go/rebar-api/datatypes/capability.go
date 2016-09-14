package datatypes

type Capability struct {
	NameID
	Description string `json:"description"`
	Source      string `json:"source"`
}

func (o *Capability) ApiName() string {
	return "capabilities"
}
