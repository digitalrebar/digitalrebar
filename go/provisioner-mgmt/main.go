package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/digitalrebar/digitalrebar/go/common/cert"
	"github.com/digitalrebar/digitalrebar/go/common/version"
	"github.com/digitalrebar/digitalrebar/go/rebar-api/client"
	"github.com/gin-gonic/gin"
	uuid "github.com/satori/go.uuid"
)

var machineKey, fileRoot, provisionerURL, commandURL string
var backEndType string
var apiPort int64
var backend storageBackend
var api *gin.Engine
var logger *log.Logger
var username, password, endpoint string
var hostString string
var versionFlag bool

func init() {
	flag.BoolVar(&versionFlag,
		"version",
		false,
		"Print Version and exit")
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
		"https://localhost:3000",
		"Public URL for the Command and Control server machines should communicate with")
	flag.StringVar(&hostString,
		"host",
		"localhost,provisioner,127.0.0.1",
		"The host IPs and names to place in the certificate.  Comma separated")

	if ep := os.Getenv("REBAR_ENDPOINT"); ep != "" {
		endpoint = ep
	}
	if kv := os.Getenv("REBAR_KEY"); kv != "" {
		key := strings.SplitN(kv, ":", 2)
		if len(key) < 2 {
			log.Fatal("REBAR_KEY does not contain a username:password pair!")
		}
		if key[0] == "" || key[1] == "" {
			log.Fatal("REBAR_KEY contains an invalid username:password pair!")
		}
		username = key[0]
		password = key[1]
	}
	flag.StringVar(&username, "username", username, "Username for Digital Rebar endpoint")
	flag.StringVar(&password, "password", password, "Password for Digital Rebar endpoint")
	flag.StringVar(&endpoint, "endpoint", endpoint, "API Endpoint for Digital Rebar")
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

	if versionFlag {
		logger.Fatalf("Version: %s", version.REBAR_VERSION)
	}

	if err := client.Session(endpoint, username, password); err != nil {
		logger.Fatalf("Could not connect to Rebar: %v", err)
	}

	logger.Printf("Version: %s\n", version.REBAR_VERSION)

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

	// Isos methods
	api.GET("/isos",
		func(c *gin.Context) {
			listIsos(c, fileRoot)
		})
	api.GET("/isos/:name",
		func(c *gin.Context) {
			getIso(c, fileRoot, c.Param(`name`))
		})
	api.POST("/isos/:name",
		func(c *gin.Context) {
			uploadIso(c, fileRoot, c.Param(`name`))
		})
	api.DELETE("/isos/:name",
		func(c *gin.Context) {
			deleteIso(c, fileRoot, c.Param(`name`))
		})
	hosts := strings.Split(hostString, ",")
	log.Fatal(cert.StartTLSServer(fmt.Sprintf(":%d", apiPort), "provisioner-mgmt", hosts, "internal", "internal", api))
}
