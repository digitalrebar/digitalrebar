package main

import (
	"github.com/ant0ine/go-json-rest/rest"
)

type dns_endpoint interface {
	GetAllZones(rest.ResponseWriter, *rest.Request)
	PostZone(rest.ResponseWriter, *rest.Request)
	GetZone(rest.ResponseWriter, *rest.Request)
	PutZone(rest.ResponseWriter, *rest.Request)
	DeleteZone(rest.ResponseWriter, *rest.Request)
	PatchZone(rest.ResponseWriter, *rest.Request)
}
