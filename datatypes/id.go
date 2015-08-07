package datatypes

import (
	"errors"
	"strconv"
)

type SimpleID struct {
	ID int64 `json:"id"`
}

var (
	IDNotSet = errors.New("ID not set")
	SetIDErr = errors.New("SetId can only be used on an un-IDed object")
)

// Id returns this attrib's ID or Name as a string.
// The REST API allows them to be used interchangeably.
func (o *SimpleID) Id() (string, error) {
	if o.ID == 0 {
		return "", IDNotSet
	}
	return strconv.FormatInt(o.ID, 10), nil
}

func (o *SimpleID) SetId(s string) error {
	if o.ID != 0 {
		return SetIDErr
	}
	id, err := strconv.ParseInt(s, 10, 64)
	o.ID = id
	return err
}

type NameID struct {
	ID   int64  `json:"id"`
	Name string `json:"name"`
}

// Id returns this attrib's ID or Name as a string.
// The REST API allows them to be used interchangeably.
func (o *NameID) Id() (string, error) {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10), nil
	} else if o.Name != "" {
		return o.Name, nil
	} else {
		return "", IDNotSet
	}
}

func (o *NameID) SetId(s string) error {
	if o.ID != 0 || o.Name != "" {
		return SetIDErr
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		o.Name = s
	}
	return nil
}
