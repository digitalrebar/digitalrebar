package datatypes

import (
	"errors"
	"regexp"
	"strconv"
)

const API_PATH = "/api/v2"

var (
	IDNotSet = errors.New("ID not set")
	SetIDErr = errors.New("SetId can only be used on an un-IDed object")
	uuidRe   = regexp.MustCompile(`^[[:xdigit:]]{8}-[[:xdigit:]]{4}-[1-5][[:xdigit:]]{3}-[89abAB][[:xdigit:]]{3}-[[:xdigit:]]{12}$`)
)

// SimpleID is used when a datatype can only be uniquely identified by its ID field.
type SimpleID struct {
	ID   int64  `json:"id"`
	UUID string `json:"uuid"`
}

// Id returns this attrib's ID or Name as a string.
// The REST API allows them to be used interchangeably.
func (o *SimpleID) Id() (string, error) {
	if o.UUID != "" {
		return o.UUID, nil
	} else if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10), nil
	}
	return "", IDNotSet
}

// SetId sets the ID.
func (o *SimpleID) SetId(s string) error {
	if o.ID != 0 || o.UUID != "" {
		return SetIDErr
	}
	if uuidRe.MatchString(s) {
		o.UUID = s
		return nil
	}
	id, err := strconv.ParseInt(s, 10, 64)
	o.ID = id
	return err
}

// NameID is used when an object can be uniquely identified by either
// its ID or its Name
type NameID struct {
	SimpleID
	Name string `json:"name"`
}

// Id returns this attrib's ID or Name as a string.
// The REST API allows them to be used interchangeably.
func (o *NameID) Id() (string, error) {
	if o.Name != "" {
		return o.Name, nil
	}
	return o.SimpleID.Id()
}

// SetId sets either the ID or the Name field, depending on whether
// the passed-in string can be parsed as an int64 or not.
func (o *NameID) SetId(s string) error {
	err := o.SimpleID.SetId(s)
	if err == nil {
		return nil
	} else if err == SetIDErr {
		return err
	}
	o.Name = s
	return nil
}
