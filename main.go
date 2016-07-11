package main

/*
Copyright (c) 2016, Rackn Inc.
Licensed under the terms of the Digital Rebar License.
See LICENSE.md at the top of this repository for more information.
*/

import (
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"flag"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"syscall"

	"github.com/ghodss/yaml"
	"github.com/gin-gonic/gin"
	"github.com/pborman/uuid"

	"github.com/coddingtonbear/go-jsonselect"
	"github.com/rackn/rule-engine/engine"
)

var (
	version                      = false
	debug                        = false
	router                       = gin.Default()
	username, password, endpoint string
	listen                       string
	backingStore                 string
	dataDir                      string
	ruleEngine                   *engine.Engine
	caCert, cert, key            string
)

func handleEvent(c *gin.Context) {
	log.Printf("Got request from %v", c.Request.RemoteAddr)
	evt := &engine.Event{}
	body, err := ioutil.ReadAll(c.Request.Body)
	if err != nil {
		log.Printf("Error reading body: %v", err)
		c.AbortWithError(http.StatusExpectationFailed, err)
		return
	}
	c.Request.Body.Close()
	if err := json.Unmarshal(body, evt); err != nil {
		log.Printf("Error decoding body: %v", err)
		log.Printf("Invalid body: %s", string(body))
		c.AbortWithError(http.StatusExpectationFailed, err)
		return
	}
	if runSync, ok := evt.Selector["sync"]; runSync == "true" && ok {
		ruleEngine.HandleEvent(evt)
		c.Status(http.StatusAccepted)
	} else {
		c.Status(http.StatusAccepted)
		go ruleEngine.HandleEvent(evt)
	}
}

func createRuleset(c *gin.Context) {
	ruleSet := engine.RuleSet{}
	body, err := ioutil.ReadAll(c.Request.Body)
	if err != nil {
		log.Printf("Error reading body: %v", err)
		c.AbortWithError(http.StatusExpectationFailed, err)
		return
	}
	c.Request.Body.Close()
	if err := yaml.Unmarshal(body, &ruleSet); err != nil {
		log.Printf("Error decoding body: %v", err)
		log.Printf("Invalid body: %s", string(body))
		c.AbortWithError(http.StatusExpectationFailed, err)
		return
	}
	ruleSet, err = ruleEngine.AddRuleSet(ruleSet)
	if err != nil {
		log.Printf("Failed to add ruleset %s: %v", ruleSet.Name, err)
		c.AbortWithError(http.StatusConflict, err)
		return
	}
	c.JSON(http.StatusCreated, ruleSet)
}

func listRulesets(c *gin.Context) {
	ruleSets := ruleEngine.RuleSets()
	c.JSON(http.StatusOK, ruleSets)
}

func showRuleset(c *gin.Context) {
	name := c.Param("name")
	rs, ok := ruleEngine.RuleSet(name)
	if !ok {
		c.AbortWithStatus(http.StatusNotFound)
		return
	}
	c.JSON(http.StatusOK, rs)
}

func updateRuleset(c *gin.Context) {
	name := c.Param("name")
	newRuleSet := engine.RuleSet{}
	body, err := ioutil.ReadAll(c.Request.Body)
	if err != nil {
		log.Printf("Error reading body: %v", err)
		c.AbortWithError(http.StatusExpectationFailed, err)
		return
	}
	c.Request.Body.Close()
	if err := yaml.Unmarshal(body, &newRuleSet); err != nil {
		log.Printf("Error decoding body: %v", err)
		log.Printf("Invalid body: %s", string(body))
		c.AbortWithError(http.StatusExpectationFailed, err)
		return
	}
	if name != newRuleSet.Name {
		log.Printf("Cannot change name from %s to %s", name, newRuleSet.Name)
		c.AbortWithStatus(http.StatusConflict)
		return
	}

	newRuleSet, err = ruleEngine.UpdateRuleSet(newRuleSet)
	if err != nil {
		log.Printf("Error updating ruleset %s: %v", name, err)
		c.AbortWithError(http.StatusConflict, err)
		return
	}
	c.JSON(http.StatusOK, newRuleSet)
}

func deleteRuleset(c *gin.Context) {
	name := c.Param("name")
	version := c.Query("version")
	if err := ruleEngine.DeleteRuleSet(name, uuid.Parse(version)); err != nil {
		c.AbortWithError(http.StatusNotFound, err)
		return
	}
	c.Status(http.StatusOK)
}

func main() {
	var err error
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
	flag.StringVar(&username, "username", "", "Username for Digital Rebar endpoint")
	flag.StringVar(&password, "password", "", "Password for Digital Rebar endpoint")
	flag.StringVar(&endpoint, "endpoint", "", "API Endpoint for Digital Rebar")
	flag.StringVar(&listen, "listen", "", "Address for the API and the event listener to listen on.")
	flag.StringVar(&caCert, "cacert", "/etc/rule-engine/cacert.pem", "Certificate to use for API and Event validation")
	flag.StringVar(&cert, "cert", "/etc/rule-engine/cert.pem", "Certificate to use for replies")
	flag.StringVar(&key, "key", "/etc/rule-engine/key.pem", "Private key for the reply cert")
	flag.StringVar(&backingStore, "backing", "file", "Backing store to use for RuleSets.  Permitted values are 'file' and 'consul'")
	flag.StringVar(&dataDir, "dataloc", "/var/cache/rule-engine", "Path to store data at")
	flag.BoolVar(&version, "version", false, "Print version and exit")
	flag.BoolVar(&debug, "debug", false, "Whether to run in debug mode")
	flag.Parse()
	if version {
		log.Fatalf("Version: 0.2.1")
	}
	if debug {
		jsonselect.EnableLogger()
	} else {
		gin.SetMode(gin.ReleaseMode)
	}

	if endpoint == "" {
		if ep := os.Getenv("REBAR_ENDPOINT"); ep != "" {
			endpoint = ep
		} else {
			log.Fatalf("No --endpoint passed and REBAR_ENDPOINT not set, aborting")
		}
	}
	if username == "" && password == "" {
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
		} else {
			log.Fatalf("No --username or --password passed, and REBAR_KEY not set")
		}
	}
	if listen == "" {
		log.Fatalf("No address to listen on passed with --listen")
	}

	var bs engine.LoadSaver
	switch backingStore {
	case "consul":
		bs, err = engine.NewConsulStore(dataDir)
	case "file":
		bs, err = engine.NewFileStore(dataDir + "/database")
	default:
		log.Fatalf("Unknown backing store %s", backingStore)
	}
	if err != nil {
		log.Fatalf("Failed to create backing store %s at %s: %v", backingStore, dataDir, err)
	}
	ruleEngine, err = engine.NewEngine(bs, listen, endpoint, username, password)
	if err != nil {
		log.Fatalf("Error creating rule engine: %v", err)
	}
	ruleEngine.Debug = debug

	// Clean up on SIGTERM
	killChan := make(chan os.Signal, 1)
	go func() {
		<-killChan
		ruleEngine.Stop()
		os.Exit(0)
	}()
	signal.Notify(killChan, syscall.SIGTERM)
	signal.Notify(killChan, syscall.SIGINT)

	router.POST("/events", handleEvent)
	router.GET("/api/v0/rulesets/", listRulesets)
	router.GET("/api/v0/rulesets/:name", showRuleset)
	router.POST("/api/v0/rulesets/", createRuleset)
	router.PUT("/api/v0/rulesets/:name", updateRuleset)
	router.DELETE("/api/v0/rulesets/:name", deleteRuleset)
	validator, err := ioutil.ReadFile(caCert)
	if err != nil {
		log.Fatalf("Error reading validation cert: %v", err)
	}
	certPool := x509.NewCertPool()
	certPool.AppendCertsFromPEM(validator)
	tlsConfig := &tls.Config{
		ClientCAs:  certPool,
		ClientAuth: tls.RequireAndVerifyClientCert,
	}
	tlsConfig.BuildNameToCertificate()

	s := &http.Server{
		Addr:      listen,
		Handler:   router,
		TLSConfig: tlsConfig,
	}

	// Wait forever
	log.Printf("Ready to handle events\n")
	for {
		log.Printf("API failed: %v", s.ListenAndServeTLS(cert, key))
	}
}
