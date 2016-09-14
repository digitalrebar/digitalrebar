This is the start of a sane-ish Rebar API in Go.

* rebar/ contains a CLI for DigitalRebar.
* client/ contains the deprecated single-use client.
* api/ contains the new multi-use client API.
* datatypes/ contains the core datatypes that Rebar uses to represent everything.

To build the CLI, just run ```go build``` in the rebar directory.

Documents are at https://godoc.org/github.com/rackn/digitalrebar/go/rebar-api/client and https://godoc.org/github.com/rackn/digitalrebar/go/rebar-api/datatypes
