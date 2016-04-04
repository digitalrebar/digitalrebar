package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/gin-gonic/gin"
	consul "github.com/hashicorp/consul/api"
	uuid "github.com/satori/go.uuid"
)

var machineKey, fileRoot, provisionerURL, commandURL string
var backEndType string
var apiPort int64
var client *consul.Client
var backend storageBackend
var api *gin.Engine
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

func popMachine(param string) *Machine {
	if _, err := uuid.FromString(param); err == nil {
		return &Machine{Uuid: param}
	} else {
		return &Machine{Name: param}
	}
}

func main() {
	// Some initial setup
	flag.Parse()
	logger = log.New(os.Stderr, "provisioner-mgmt", log.LstdFlags|log.Lmicroseconds|log.LUTC)
	var err error
	switch backEndType {
	case "consul":
		backend, err = newConsulBackend(machineKey)
	case "directory":
		backend, err = newFileBackend(machineKey)
	default:
		logger.Fatalf("Unknown storage backend type %v\n", backEndType)
	}
	api := gin.Default()
	if err != nil {
		logger.Fatal(err)
	}
	// bootenv methods
	api.GET("/bootenvs",
		func(c *gin.Context) {
			listThings(c, &BootEnv{})
		})
	api.POST("/bootenvs",
		func(c *gin.Context) {
			createThing(c, &BootEnv{})
		})
	api.GET("/bootenvs/:name",
		func(c *gin.Context) {
			getThing(c, &BootEnv{Name: c.Param(`name`)})
		})
	api.PATCH("/bootenvs/:name",
		func(c *gin.Context) {
			updateThing(c, &BootEnv{Name: c.Param(`name`)}, &BootEnv{})
		})
	api.DELETE("/bootenvs/:name",
		func(c *gin.Context) {
			deleteThing(c, &BootEnv{Name: c.Param(`name`)})
		})
	// machine methods
	api.GET("/machines",
		func(c *gin.Context) {
			listThings(c, &Machine{})
		})
	api.POST("/machines",
		func(c *gin.Context) {
			createThing(c, &Machine{})
		})
	api.GET("/machines/:name", func(c *gin.Context) {
		getThing(c, popMachine(c.Param(`name`)))
	})
	api.PATCH("/machines/:name",
		func(c *gin.Context) {
			updateThing(c, popMachine(c.Param(`name`)), &Machine{})
		})
	api.DELETE("/machines/:name",
		func(c *gin.Context) {
			deleteThing(c, popMachine(c.Param(`name`)))
		})

	// template methods
	api.GET("/templates",
		func(c *gin.Context) {
			listThings(c, &Template{})
		})
	api.POST("/templates",
		func(c *gin.Context) {
			createThing(c, &Template{})
		})
	api.POST("/templates/:uuid", createTemplate)
	api.GET("/templates/:uuid",
		func(c *gin.Context) {
			getThing(c, &Template{UUID: c.Param(`uuid`)})
		})
	api.PATCH("/templates/:uuid",
		func(c *gin.Context) {
			updateThing(c, &Template{UUID: c.Param(`uuid`)}, &Template{})
		})
	api.DELETE("/templates/:uuid",
		func(c *gin.Context) {
			deleteThing(c, &Template{UUID: c.Param(`uuid`)})
		})

	api.Run(fmt.Sprintf(":%d", apiPort))
}
