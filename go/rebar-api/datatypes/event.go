package datatypes

type Event struct {
	SimpleID
	EventSelectors []string               `json:"event_selectors"`
	Note           string                 `json:"note"`
	TargetClass    string                 `json:"target_class"`
	Params         map[string]interface{} `json:"params"`
	Target         map[string]interface{} `json:"target"`
}

func (o *Event) ApiName() string {
	return "events"
}

// AddressID is used for NetworkAllocation and NetworkRouter, as they
// can be uniquely identified by either their ID or Address fields.
type EventSinkID struct {
	SimpleID
	// Address is an IPv4 or v6 address in CIDR format.
	Endpoint string `json:"endpoint"`
}

// Id returns this attrib's ID.
func (o *EventSinkID) Id() (string, error) {
	return o.SimpleID.Id()
}

// SetId sets either the ID or the Name field, depending on whether
// the passed-in string can be parsed as an int64 or not.
func (o *EventSinkID) SetId(s string) error {
	err := o.SimpleID.SetId(s)
	if err == nil {
		return nil
	} else if err == SetIDErr {
		return err
	}
	o.Endpoint = s
	return nil
}

type EventSink struct {
	EventSinkID
	Username      string `json:"username"`
	Authenticator string `json:"authenticator"`
	Notes         string `json:"notes"`
	TenantID      int64  `json:"tenant_id,omitempty"`
}

func (o *EventSink) ApiName() string {
	return "event_sinks"
}

type EventSelector struct {
	SimpleID
	EventSinkID int64                  `json:"event_sink_id"`
	TenantID    int64                  `json:"tenant_id,omitempty"`
	Selector    map[string]interface{} `json:"selector"`
}

func (o *EventSelector) ApiName() string {
	return "event_selectors"
}
