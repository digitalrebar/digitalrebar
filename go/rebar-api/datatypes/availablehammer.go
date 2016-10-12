package datatypes

import "path"

// AvailableHammer tracks the hammers that can be bound to a Node.
type AvailableHammer struct {
	NameID
	Priority int64  `json:"priority"`
	Type     string `json:"klass"`
}

func (o *AvailableHammer) ApiName() string {
	return "available_hammers"
}

func (o *AvailableHammer) ApiPath() string {
	return path.Join(API_PATH, o.ApiName())
}
