package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/digitalrebar/digitalrebar/go/common/cert"
	"github.com/digitalrebar/digitalrebar/go/common/client"
	"github.com/digitalrebar/digitalrebar/go/common/service"
	"github.com/digitalrebar/digitalrebar/go/common/version"
	"github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	"github.com/gin-gonic/gin"
	consul "github.com/hashicorp/consul/api"
	uuid "github.com/satori/go.uuid"
)

var machineKey, fileRoot, provisionerURL, commandURL, ourAddress string
var backEndType string
var apiPort, staticPort, tftpPort int
var backend storageBackend
var rebarClient *api.Client
var logger *log.Logger
var username, password, endpoint string
var hostString string
var versionFlag bool

func popMachine(param string) *Machine {
	if _, err := uuid.FromString(param); err == nil {
		return &Machine{Uuid: param}
	} else {
		return &Machine{Name: param}
	}
}

func main() {
	// Some initial setup
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
	flag.IntVar(&apiPort,
		"api-port",
		8092,
		"Port the HTTP API should listen on")
	flag.IntVar(&staticPort,
		"static-port",
		8091,
		"Port the static HTTP file server should listen on")
	flag.IntVar(&tftpPort,
		"tftp-port",
		69,
		"Port for the TFTP server to listen on")
	flag.StringVar(&fileRoot,
		"file-root",
		"/tftpboot",
		"Root of filesystem we should manage")
	flag.Parse()
	logger = log.New(os.Stderr, "provisioner-mgmt", log.LstdFlags|log.Lmicroseconds|log.LUTC)

	if versionFlag {
		logger.Fatalf("Version: %s", version.REBAR_VERSION)
	}

	commandURL = os.Getenv("EXTERNAL_REBAR_ENDPOINT")

	if commandURL == "" {
		logger.Fatalln("EXTERNAL_REBAR_ENDPOINT not set!")
	}

	logger.Printf("Version: %s\n", version.REBAR_VERSION)

	var err error
	consulClient, err := client.Consul(true)
	if err != nil {
		logger.Fatalf("Error talking to Consul: %v", err)
	}

	rebarClient, err = api.TrustedSession("system", true)
	if err != nil {
		logger.Fatalf("Error creating trusted Rebar API client: %v", err)
	}

	// Register service with Consul before continuing
	if err = service.Register(consulClient,
		&consul.AgentServiceRegistration{
			Name: "provisioner-service",
			Tags: []string{"deployment:system"},
			Port: staticPort,
			Check: &consul.AgentServiceCheck{
				HTTP:     fmt.Sprintf("http://[::]:%d/", staticPort),
				Interval: "10s",
			},
		},
		true); err != nil {
		log.Fatalf("Failed to register provisioner-service with Consul: %v", err)
	}

	if err = service.Register(consulClient,
		&consul.AgentServiceRegistration{
			Name: "provisioner-mgmt-service",
			Tags: []string{"revproxy"}, // We want to be exposed through the revproxy
			Port: apiPort,
			Check: &consul.AgentServiceCheck{
				HTTP:     fmt.Sprintf("http://[::]:%d/", staticPort),
				Interval: "10s",
			},
		},
		false); err != nil {
		log.Fatalf("Failed to register provisioner-mgmt-service with Consul: %v", err)
	}
	if err = service.Register(consulClient,
		&consul.AgentServiceRegistration{
			Name: "provisioner-tftp-service",
			Port: tftpPort,
			Check: &consul.AgentServiceCheck{
				HTTP:     fmt.Sprintf("http://[::]:%d/", staticPort),
				Interval: "10s",
			},
		},
		true); err != nil {
		log.Fatalf("Failed to register provisioner-tftp-service with Consul: %v", err)
	}
	// Figure out our service address

	for {
		svc, err := service.Find(consulClient, "provisioner", "")
		if err != nil {
			log.Fatalf("Error talking to Consul: %v", err)
		}
		if len(svc) == 0 {
			continue
		}

		ourAddress, _ = service.Address(svc[0])
		provisionerURL = fmt.Sprintf("http://%s:%d", ourAddress, staticPort)
		break
	}

	switch backEndType {
	case "consul":
		backend, err = newConsulBackend(machineKey)
	case "directory":
		backend, err = newFileBackend(machineKey)
	default:
		logger.Fatalf("Unknown storage backend type %v\n", backEndType)
	}

	mgmtApi := gin.Default()
	if err != nil {
		logger.Fatal(err)
	}
	// bootenv methods
	mgmtApi.GET("/bootenvs",
		func(c *gin.Context) {
			listThings(c, &BootEnv{})
		})
	mgmtApi.POST("/bootenvs",
		func(c *gin.Context) {
			createThing(c, &BootEnv{})
		})
	mgmtApi.GET("/bootenvs/:name",
		func(c *gin.Context) {
			getThing(c, &BootEnv{Name: c.Param(`name`)})
		})
	mgmtApi.PATCH("/bootenvs/:name",
		func(c *gin.Context) {
			updateThing(c, &BootEnv{Name: c.Param(`name`)}, &BootEnv{})
		})
	mgmtApi.DELETE("/bootenvs/:name",
		func(c *gin.Context) {
			deleteThing(c, &BootEnv{Name: c.Param(`name`)})
		})
	// machine methods
	mgmtApi.GET("/machines",
		func(c *gin.Context) {
			listThings(c, &Machine{})
		})
	mgmtApi.POST("/machines",
		func(c *gin.Context) {
			createThing(c, &Machine{})
		})
	mgmtApi.GET("/machines/:name", func(c *gin.Context) {
		getThing(c, popMachine(c.Param(`name`)))
	})
	mgmtApi.PATCH("/machines/:name",
		func(c *gin.Context) {
			updateThing(c, popMachine(c.Param(`name`)), &Machine{})
		})
	mgmtApi.DELETE("/machines/:name",
		func(c *gin.Context) {
			deleteThing(c, popMachine(c.Param(`name`)))
		})

	// template methods
	mgmtApi.GET("/templates",
		func(c *gin.Context) {
			listThings(c, &Template{})
		})
	mgmtApi.POST("/templates",
		func(c *gin.Context) {
			createThing(c, &Template{})
		})
	mgmtApi.POST("/templates/:uuid", createTemplate)
	mgmtApi.GET("/templates/:uuid",
		func(c *gin.Context) {
			getThing(c, &Template{UUID: c.Param(`uuid`)})
		})
	mgmtApi.PATCH("/templates/:uuid",
		func(c *gin.Context) {
			updateThing(c, &Template{UUID: c.Param(`uuid`)}, &Template{})
		})
	mgmtApi.DELETE("/templates/:uuid",
		func(c *gin.Context) {
			deleteThing(c, &Template{UUID: c.Param(`uuid`)})
		})

	// Isos methods
	mgmtApi.GET("/isos",
		func(c *gin.Context) {
			listIsos(c, fileRoot)
		})
	mgmtApi.GET("/isos/:name",
		func(c *gin.Context) {
			getIso(c, fileRoot, c.Param(`name`))
		})
	mgmtApi.POST("/isos/:name",
		func(c *gin.Context) {
			uploadIso(c, fileRoot, c.Param(`name`))
		})
	mgmtApi.DELETE("/isos/:name",
		func(c *gin.Context) {
			deleteIso(c, fileRoot, c.Param(`name`))
		})

	s, err := cert.Server("internal", "provisioner-mgmt-service")
	if err != nil {
		log.Fatalf("Error creating trusted server: %v", err)
	}
	s.Addr = fmt.Sprintf(":%d", apiPort)
	s.Handler = mgmtApi

	go func() {
		if err = s.ListenAndServeTLS("", ""); err != nil {
			log.Fatalf("Error running API service: %v", err)
		}
	}()
	if err = serveTftp(fmt.Sprintf(":%d", tftpPort)); err != nil {
		log.Fatalf("Error starting TFTP server: %v", err)
	}
	// Static file server must always be last, as all our health checks key off of it.
	if err = serveStatic(fmt.Sprintf(":%d", staticPort), fileRoot); err != nil {
		log.Fatalf("Error starting static file server: %v", err)
	}
}
