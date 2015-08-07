package crowbar

import (
	"encoding/json"
	"fmt"
	"log"
	"path"

	"github.com/VictorLowther/crowbar-api/datatypes"
)

type Attrib struct {
	datatypes.Attrib
	Timestamps
	apiHelper
}

// Attriber defines what is needed to get and set attribs on an object.
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
func Attribs(scope ...Attriber) (res []*Attrib, err error) {
	res = make([]*Attrib, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}
	paths = append(paths, "attribs")
	return res, List(path.Join(paths...), &res)
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
func GetAttrib(o Attriber, a *Attrib, bucket string) (res *Attrib, err error) {
	res = &Attrib{}
	id, err := a.Id()
	if err == datatypes.IDNotSet {
		log.Panic(err)
	}
	res.SetId(id)
	uri := url(o, url(res))
	res.setLastFetch(uri)
	if bucket != "" {
		uri = fmt.Sprintf("%v?bucket=%v", uri, bucket)
	}
	inbuf, err := json.Marshal(res)
	if err != nil {
		return res, err
	}
	outbuf, err := session.request("GET", uri, inbuf)
	if err != nil {
		return res, err
	}
	return res, unmarshal(uri, outbuf, res)
}

// SetAttrib sets the value of an attrib in the context of
// an attriber in the passed bucket.  Valid buckets are:
//
//    * "user"
//    * "note"
func SetAttrib(o Attriber, a *Attrib, bucket string) error {
	if bucket == "" {
		bucket = "user"
	}
	uri := url(o, url(a))
	a.setLastFetch(uri)
	uri = fmt.Sprintf("%v?bucket=%v", uri, bucket)
	patch, err := MakePatch(a)
	if err != nil {
		return err
	}
	outbuf, err := session.request("PATCH", uri, patch)
	if err != nil {
		return err
	}
	return unmarshal(uri, outbuf, a)
}

// Propose readies an Attriber to accept new values via SetAttrib.
func Propose(o Attriber) error {
	inbuf, err := json.Marshal(o)
	if err != nil {
		return err
	}
	outbuf, err := session.request("PUT", url(o, "propose"), inbuf)
	if err != nil {
		return err
	}
	return unmarshal(url(o), outbuf, o)
}

// Commit makes the values set on the Attriber via SetAttrib visible
// to the rest of the Crowbar infrastructure.
func Commit(o Attriber) error {
	inbuf, err := json.Marshal(o)
	if err != nil {
		return err
	}
	outbuf, err := session.request("PUT", url(o, "commit"), inbuf)
	if err != nil {
		return err
	}
	return unmarshal(url(o), outbuf, o)
}
