package api

import (
	"fmt"
	"log"
	"path"

	"github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"
)

// Attrib wraps datatypes.Attrib with the needed functionality for the
// client side API
type Attrib struct {
	datatypes.Attrib
	Timestamps
	apiHelper
}

// Attriber defines what is needed to get and set attribs on an
// object.
type Attriber interface {
	// You must be a Crudder to be an Attriber.
	Crudder
	// satisfy the Attriber interface.
	attribs()
}

// Attribs gets all the Attribs from a location in the API.  If paths
// is empty, then all Attribs will be fetched, otherwise the paths
// will be joined with "attribs" and we will attempt to fetch the
// Attribs from there.  This behaviour exists because the REST API
// allows you to perform scoped fetches.
func (c *Client) Attribs(scope ...Attriber) (res []*Attrib, err error) {
	res = make([]*Attrib, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	paths = append(paths, "attribs")
	return res, c.List(path.Join(datatypes.API_PATH, path.Join(paths...)), &res)
}

// GetAttrib gets an attrib in the context of an Attriber.  The
// returned Attrib will have its value populated from the contents of
// the passed bucket.  Valid buckets are:
//
//    * "proposed"
//    * "committed"
//    * "system"
//    * "wall"
//    * "note"
//    * "all"
func (c *Client) GetAttrib(o Attriber, a *Attrib, bucket string) (res *Attrib, err error) {
	res = &Attrib{}
	id, err := a.Id()
	if err == datatypes.IDNotSet {
		log.Panic(err)
	}
	res.SetId(id)
	uri := urlFor(o, fragTo(res))
	if bucket != "" {
		uri = fmt.Sprintf("%v?bucket=%v", uri, bucket)
	}
	outbuf, err := c.request("GET", uri, nil)
	if err != nil {
		return res, err
	}
	return res, c.unmarshal(uri, outbuf, res)
}

// FetchAttrib behaves the same as GetAttrib, but accepts the name of an
// attrib instead of an attrib.
func (c *Client) FetchAttrib(o Attriber, attr, bucket string) (res *Attrib, err error) {
	res = &Attrib{}
	res.SetId(attr)
	return c.GetAttrib(o, res, bucket)
}

// SetAttrib sets the value of an attrib in the context of
// an attriber in the passed bucket.  Valid buckets are:
//
//    * "user"
//    * "note"
func (c *Client) SetAttrib(o Attriber, a *Attrib, bucket string) error {
	if bucket == "" {
		bucket = "user"
	}
	uri := urlFor(o, fragTo(a))
	uri = fmt.Sprintf("%v?bucket=%v", uri, bucket)
	patch, err := MakePatch(a)
	if err != nil {
		return err
	}
	outbuf, err := c.request("PATCH", uri, patch)
	if err != nil {
		return err
	}
	return c.unmarshal(uri, outbuf, a)
}

// Propose readies an Attriber to accept new values via SetAttrib.
func (c *Client) Propose(o Attriber) error {
	outbuf, err := c.request("PUT", urlFor(o, "propose"), nil)
	if err != nil {
		return err
	}
	return c.unmarshal(urlFor(o), outbuf, o)
}

// Commit makes the values set on the Attriber via SetAttrib visible
// to the rest of the Rebar infrastructure.
func (c *Client) Commit(o Attriber) error {
	outbuf, err := c.request("PUT", urlFor(o, "commit"), nil)
	if err != nil {
		return err
	}
	return c.unmarshal(urlFor(o), outbuf, o)
}
