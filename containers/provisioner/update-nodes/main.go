package main

import (
	"crypto/tls"
	"crypto/x509"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
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
var cacert, cert, key string

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
		"https://localhost:3000",
		"Public URL for the Command and Control server machines should communicate with")
	flag.StringVar(&cacert,
		"cacert",
		"/etc/prov-base-cert.pem",
		"Certificate to use for validation")
	flag.StringVar(&cert,
		"cert",
		"/etc/prov-cert.pem",
		"Certificate to use for replies")
	flag.StringVar(&key,
		"key",
		"/etc/prov-key.pem",
		"Private Key to use for replies")
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

	caCert, err := ioutil.ReadFile(cacert)
	if err != nil {
		log.Fatal(err)
	}
	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM(caCert)

	// Setup HTTPS client
	tlsConfig := &tls.Config{
		ClientCAs: caCertPool,
		// NoClientCert
		// RequestClientCert
		// RequireAnyClientCert
		// VerifyClientCertIfGiven
		// RequireAndVerifyClientCert
		ClientAuth: tls.RequireAndVerifyClientCert,
	}
	tlsConfig.BuildNameToCertificate()

	s := &http.Server{
		Addr:    fmt.Sprintf(":%d", apiPort),
		Handler: api,
	}
	s.TLSConfig = tlsConfig

	log.Fatal(s.ListenAndServeTLS(cert, key))
}
