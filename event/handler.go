package event

import (
	"encoding/json"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"reflect"

	"github.com/digitalrebar/rebar-api/api"
)

// Handler describes the interface that anything that handles Events should satisfy.
type Handler interface {
	HandleEvent(*Event) error
}

// Sink implements some basic functionality for acting as an event sink.
type Sink struct {
	s *api.EventSink
	h Handler
}

// NewSink creates a new Sink.  It handles registering an EventSink if needed.
func NewSink(c *api.Client, uri string, h Handler) (*Sink, error) {
	sink := &Sink{}
	sink.s = &api.EventSink{}
	sink.h = h
	matcher := map[string]interface{}{"endpoint": uri}
	matches := []*api.EventSink{}
	if err := c.Match(sink.s.ApiName(), matcher, &matches); err != nil {
		return nil, err
	}

	switch len(matches) {
	case 0:
		sink.s.Endpoint = uri
		return sink, c.BaseCreate(sink.s)
	case 1:
		sink.s = matches[0]
		return sink, nil
	default:
		log.Panicf("%d sinks registered for %v, cannot happen!", len(matches), uri)
		return nil, nil
	}

}

// ServeHTTP allows a Sink to hook together an http Server and
// something we want to handle events.
func (s *Sink) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	evt := &Event{}
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		w.Header().Set("Content-Type", "application/json; charset=UTF-8")
		w.WriteHeader(http.StatusBadRequest)
		io.WriteString(w, `{"status": 400,"message":"Unable to read request body"}`)
		return
	}
	r.Body.Close()
	if err := json.Unmarshal(body, evt); err != nil {
		w.Header().Set("Content-Type", "application/json; charset=UTF-8")
		w.WriteHeader(http.StatusBadRequest)
		io.WriteString(w, `{"status":400,"message":"Body not valid JSON"}`)
		return
	}
	err = nil
	if runSync, ok := evt.Selector["sync"]; ok {
		if rs, ok := runSync.(bool); ok && rs {
			err = s.h.HandleEvent(evt)
			if err == nil {
				w.WriteHeader(http.StatusOK)
				return
			}
			w.Header().Set("Content-Type", "application/json; charset=UTF-8")
			ret := map[string]interface{}{
				"status":  http.StatusExpectationFailed,
				"message": err.Error(),
			}
			w.WriteHeader(http.StatusExpectationFailed)
			buf, _ := json.Marshal(ret)
			w.Write(buf)
			return
		}
	}
	w.WriteHeader(http.StatusAccepted)
	go s.h.HandleEvent(evt)
}

// EventSelectors gets the currently registered EventSelectors for this Sink.
func (s *Sink) EventSelectors(c *api.Client) ([]*api.EventSelector, error) {
	matcher := map[string]interface{}{"event_sink_id": s.s.ID}
	matches := []*api.EventSelector{}
	selector := &api.EventSelector{}
	err := c.Match(selector.ApiName(), matcher, &matches)
	return matches, err
}

// Stop deregisters this EventSink and associated EventSelectors
func (s *Sink) Stop(c *api.Client) error {
	if err := s.SetSelectors(c, []Selector{}); err != nil {
		return err
	}
	return c.Destroy(s.s)
}

// SetSelectors sets the registered EventSelectors for this EventSink
// to match the wanted slice.  It takes care of adding and deleting
// api.EventSelectors as needed
func (s *Sink) SetSelectors(c *api.Client, wanted []Selector) error {
	currentSelectors, err := s.EventSelectors(c)
	if err != nil {
		return err
	}
	unchanged := []*api.EventSelector{}
	// Add new selectors
	for _, sel := range wanted {
		doAdd := true
		for _, es := range currentSelectors {
			if reflect.DeepEqual(es.Selector, sel) {
				doAdd = false
				unchanged = append(unchanged, es)
				break
			}
		}
		if doAdd {
			newSelector := &api.EventSelector{}
			newSelector.EventSinkID = s.s.ID
			newSelector.Selector = map[string]interface{}(sel)
			if err := c.BaseCreate(newSelector); err != nil {
				return err
			}
		}
	}

	// Delete old ones
	for _, es := range currentSelectors {
		doDelete := true
		for _, sel := range unchanged {
			if es == sel {
				doDelete = false
				break
			}
		}
		if doDelete {
			if err := c.Destroy(es); err != nil {
				return err
			}
		}
	}
	return nil
}
