package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	consul "github.com/hashicorp/consul/api"
	"github.com/labstack/echo"
	mw "github.com/labstack/echo/middleware"
)

var nodeKey, fileRoot string
var backEndType string
var apiPort int64
var client *consul.Client
var backend storageBackend
var api *echo.Echo

func init() {
	flag.StringVar(&backEndType,
		"backend",
		"consul",
		"Storage backend to use.  Can be either 'consul' or 'directory'")
	flag.StringVar(&nodeKey,
		"data-root",
		"digitalrebar/provisioner/boot-info",
		"Location we should store runtime information in")
	flag.Int64Var(&apiPort,
		"api-port",
		8092,
		"Port the HTTP API should listen on")
	flag.StringVar(&fileRoot,
		"file-root",
		"/tftpboot",
		"Root of filesystem we should manage")
}

func main() {
	// Some initial setup
	flag.Parse()
	var err error
	switch backEndType {
	case "consul":
		backend, err = NewConsulBackend(nodeKey)
	case "directory":
		backend, err = NewFileBackend(nodeKey)
	default:
		log.Fatalf("Unknown storage backend type %v\n", backEndType)
	}
	api = echo.New()
	api.Use(mw.Logger())
	api.Use(mw.Recover())
	api.SetLogOutput(os.Stderr)
	api.SetDebug(true)
	if err != nil {
		log.Panic(err)
	}
	// bootenv methods
	api.Get("/bootenvs",
		func(c *echo.Context) error {
			return listThings(c, &BootEnv{})
		})
	api.Post("/bootenvs",
		func(c *echo.Context) error {
			return createThing(c, &BootEnv{})
		})
	api.Get("/bootenvs/:name",
		func(c *echo.Context) error {
			return getThing(c, &BootEnv{Name: c.P(0)})
		})
	api.Patch("/bootenvs/:name",
		func(c *echo.Context) error {
			return updateThing(c, &BootEnv{Name: c.P(0)}, &BootEnv{})
		})
	api.Delete("/bootenvs/:name",
		func(c *echo.Context) error {
			return deleteThing(c, &BootEnv{Name: c.P(0)})
		})
	// node methods
	api.Get("/nodes",
		func(c *echo.Context) error {
			return listThings(c, &Node{})
		})
	api.Post("/nodes",
		func(c *echo.Context) error {
			return createThing(c, &Node{})
		})
	api.Get("/nodes/:name",
		func(c *echo.Context) error {
			return getThing(c, &Node{Name: c.P(0)})
		})
	api.Patch("/nodes/:name",
		func(c *echo.Context) error {
			return updateThing(c, &Node{Name: c.P(0)}, &Node{})
		})
	api.Delete("/nodes/:name",
		func(c *echo.Context) error {
			return deleteThing(c, &Node{Name: c.P(0)})
		})
	api.Run(fmt.Sprintf(":%d", apiPort))
}
