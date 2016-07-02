package datatypes

type Capability struct {
	SimpleID
	Name        string `json:"name"`
	Description string `json:"description"`
	Source      string `json:"source"`
}

func (o *Capability) ApiName() string {
	return "capabilities"
}
