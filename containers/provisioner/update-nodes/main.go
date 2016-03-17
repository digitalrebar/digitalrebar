package main

import (
	"flag"
	"fmt"
	"os"

	consul "github.com/hashicorp/consul/api"
	"github.com/labstack/echo"
	"github.com/labstack/echo/engine/standard"
	mw "github.com/labstack/echo/middleware"
	"github.com/labstack/gommon/log"
)

var machineKey, fileRoot, provisionerURL, commandURL string
var backEndType string
var apiPort int64
var client *consul.Client
var backend storageBackend
var api *echo.Echo
var logger *log.Logger

func init() {
	flag.StringVar(&backEndType,
		"backend",
		"consul",
		"Storage backend to use.  Can be either 'consul' or 'directory'")
	flag.StringVar(&machineKey,
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
	flag.StringVar(&provisionerURL,
		"provisioner",
		"http://localhost:8091",
		"Public URL for the provisioner")
	flag.StringVar(&commandURL,
		"command",
		"http://localhost:3000",
		"Public URL for the Command and Control server machines should communicate with")
}

func main() {
	// Some initial setup
	flag.Parse()
	logger = log.New("provisioner-mgmt")
	logger.SetOutput(os.Stderr)
	var err error
	switch backEndType {
	case "consul":
		backend, err = newConsulBackend(machineKey)
	case "directory":
		backend, err = newFileBackend(machineKey)
	default:
		logger.Fatalf("Unknown storage backend type %v\n", backEndType)
	}
	api = echo.New()
	api.Use(mw.Logger())
	api.Use(mw.Recover())
	api.SetLogOutput(os.Stderr)
	api.SetDebug(true)
	if err != nil {
		logger.Fatal(err)
	}
	// bootenv methods
	api.Get("/bootenvs",
		echo.HandlerFunc(func(c echo.Context) error {
			return listThings(c, &BootEnv{})
		}))
	api.Post("/bootenvs",
		echo.HandlerFunc(func(c echo.Context) error {
			return createThing(c, &BootEnv{})
		}))
	api.Get("/bootenvs/:name",
		echo.HandlerFunc(func(c echo.Context) error {
			return getThing(c, &BootEnv{Name: c.P(0)})
		}))
	api.Patch("/bootenvs/:name",
		echo.HandlerFunc(func(c echo.Context) error {
			return updateThing(c, &BootEnv{Name: c.P(0)}, &BootEnv{})
		}))
	api.Delete("/bootenvs/:name",
		echo.HandlerFunc(func(c echo.Context) error {
			return deleteThing(c, &BootEnv{Name: c.P(0)})
		}))
	// machine methods
	api.Get("/machines",
		echo.HandlerFunc(func(c echo.Context) error {
			return listThings(c, &Machine{})
		}))
	api.Post("/machines",
		echo.HandlerFunc(func(c echo.Context) error {
			return createThing(c, &Machine{})
		}))
	api.Get("/machines/:name",
		echo.HandlerFunc(func(c echo.Context) error {
			return getThing(c, &Machine{Name: c.P(0)})
		}))
	api.Patch("/machines/:name",
		echo.HandlerFunc(func(c echo.Context) error {
			return updateThing(c, &Machine{Name: c.P(0)}, &Machine{})
		}))
	api.Delete("/machines/:name",
		echo.HandlerFunc(func(c echo.Context) error {
			return deleteThing(c, &Machine{Name: c.P(0)})
		}))

	// template methods
	api.Get("/templates",
		echo.HandlerFunc(func(c echo.Context) error {
			return listThings(c, &Template{})
		}))
	api.Post("/templates",
		echo.HandlerFunc(func(c echo.Context) error {
			return createThing(c, &Template{})
		}))
	api.Post("/templates/:uuid", echo.HandlerFunc(createTemplate))
	api.Get("/templates/:uuid",
		echo.HandlerFunc(func(c echo.Context) error {
			return getThing(c, &Template{UUID: c.P(0)})
		}))
	api.Patch("/templates/:uuid",
		echo.HandlerFunc(func(c echo.Context) error {
			return updateThing(c, &Template{UUID: c.P(0)}, &Template{})
		}))
	api.Delete("/templates/:uuid",
		echo.HandlerFunc(func(c echo.Context) error {
			return deleteThing(c, &Template{UUID: c.P(0)})
		}))

	api.Run(standard.New(fmt.Sprintf(":%d", apiPort)))
}
