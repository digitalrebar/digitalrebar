package main

import (
	"github.com/ant0ine/go-json-rest/rest"
)

type backendError struct {
	s           string
	status_code int
}

func (e *backendError) Error() string {
	return e.s
}

func (e *backendError) StatusCode() int {
	return e.status_code
}

type dns_backend_point interface {
	GetAllZones() ([]Zone, *backendError)
	PostZone(Zone) (Zone, *backendError)
	GetZone(string) (Zone, *backendError)
	PutZone(string, Zone) (Zone, *backendError)
	DeleteZone(string) *backendError
	PatchZone(string, RRSets) (Zone, *backendError)
}

type dns_frontend_point interface {
	GetAllZones(rest.ResponseWriter, *rest.Request)
	PostZone(rest.ResponseWriter, *rest.Request)
	GetZone(rest.ResponseWriter, *rest.Request)
	PutZone(rest.ResponseWriter, *rest.Request)
	DeleteZone(rest.ResponseWriter, *rest.Request)
	PatchZone(rest.ResponseWriter, *rest.Request)
}
