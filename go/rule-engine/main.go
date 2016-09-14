package main

/*
Copyright (c) 2016, Rackn Inc.
Licensed under the terms of the Digital Rebar License.
See LICENSE.md at the top of this repository for more information.
*/

import (
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strconv"
	"syscall"

	"github.com/ghodss/yaml"
	"github.com/gin-gonic/gin"
	"github.com/pborman/uuid"

	"github.com/coddingtonbear/go-jsonselect"
	"github.com/rackn/digitalrebar/go/common/cert"
	"github.com/rackn/digitalrebar/go/common/client"
	multitenancy "github.com/rackn/digitalrebar/go/common/multi-tenancy"
	"github.com/rackn/digitalrebar/go/common/service"
	"github.com/rackn/digitalrebar/go/common/store"
	rbversion "github.com/rackn/digitalrebar/go/common/version"
	"github.com/rackn/digitalrebar/go/rebar-api/api"
	"github.com/rackn/digitalrebar/go/rule-engine/engine"
	consul "github.com/hashicorp/consul/api"
)

var (
	version      = false
	rbvFlag      = false
	debug        = false
	router       = gin.Default()
	listen       string
	backingStore string
	dataDir      string
	ruleEngine   *engine.Engine
)

func testCap(c *gin.Context, rs *engine.RuleSet, op string) bool {
	cap, capOK := c.Get("Capabilities")
	if !capOK {
		return false
	}
	capSet, ok := cap.(multitenancy.CapabilityMap)
	if !ok {
		return false
	}
	return capSet.HasCapability(int(rs.TenantID), op)
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
	name, ok := c.Get("User")
	if !ok {
		c.AbortWithError(http.StatusExpectationFailed, errors.New("Failed to fetch user"))
	}
	n, ok := name.(string)
	if !ok {
		c.AbortWithError(http.StatusExpectationFailed,
			errors.New("Failed to fetch user"))
	}
	ruleSet.Username = n
	user := &api.User{}
	if err := ruleEngine.Client.Fetch(user, ruleSet.Username); err != nil {
		c.AbortWithError(http.StatusExpectationFailed,
			fmt.Errorf("Failed to fetch user: %v", err))
	}
	ruleSet.TenantID = user.CurrentTenantID
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
	toShow := []engine.RuleSet{}
	for i := range ruleSets {
		if testCap(c, &ruleSets[i], "RULESET_READ") {
			toShow = append(toShow, ruleSets[i])
		}
	}
	c.JSON(http.StatusOK, toShow)
}

func showRuleset(c *gin.Context) {
	name := c.Param("name")
	rs, ok := ruleEngine.RuleSet(name)
	if !(ok && testCap(c, &rs, "RULESET_READ")) {
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
	oldRuleSet, ok := ruleEngine.RuleSet(name)
	if !(ok &&
		testCap(c, &oldRuleSet, "RULESET_READ") &&
		testCap(c, &oldRuleSet, "RULESET_UPDATE")) {
		c.AbortWithStatus(http.StatusForbidden)
		return
	}
	if name != newRuleSet.Name {
		log.Printf("Cannot change name from %s to %s", name, newRuleSet.Name)
		c.AbortWithStatus(http.StatusConflict)
		return
	}
	if oldRuleSet.TenantID != newRuleSet.TenantID {
		log.Printf("Cannot change tenant ID from %d to %s for ruleset %s",
			oldRuleSet.TenantID,
			newRuleSet.TenantID,
			name)
		c.AbortWithStatus(http.StatusConflict)
		return
	}
	if oldRuleSet.Username != newRuleSet.Username {
		log.Printf("Cannot change username from %s to %s for ruleset %s",
			oldRuleSet.Username,
			newRuleSet.Username,
			name)
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
	oldRuleSet, ok := ruleEngine.RuleSet(name)
	if !(ok &&
		testCap(c, &oldRuleSet, "RULESET_READ") &&
		testCap(c, &oldRuleSet, "RULESET_UPDATE")) {
		c.AbortWithStatus(http.StatusForbidden)
		return
	}
	version := c.Query("version")
	if err := ruleEngine.DeleteRuleSet(name, uuid.Parse(version)); err != nil {
		c.AbortWithError(http.StatusNotFound, err)
		return
	}
	c.Status(http.StatusOK)
}

func capMiddleware(c *gin.Context) {
	cmap, err := multitenancy.NewCapabilityMap(c.Request)
	if err != nil {
		c.AbortWithError(http.StatusPreconditionFailed, err)
	}
	c.Set("Capabilities", cmap)
	user := c.Request.Header.Get("X-Authenticated-Username")
	if user == "" {
		c.AbortWithError(http.StatusPreconditionFailed, errors.New("No username fetched"))
	}
	c.Set("User", user)
	c.Next()
}

func main() {
	var err error
	flag.StringVar(&backingStore, "backing", "file", "Backing store to use for RuleSets.  Permitted values are 'file' and 'consul'")
	flag.StringVar(&dataDir, "dataloc", "/var/cache/rule-engine", "Path to store data at")
	flag.BoolVar(&version, "version", false, "Print version and exit")
	flag.BoolVar(&rbvFlag, "rbv", false, "Print rebar version and exit")
	flag.BoolVar(&debug, "debug", false, "Whether to run in debug mode")
	flag.Parse()
	if version {
		log.Fatalf("Version: 0.2.1")
	}
	if rbvFlag {
		log.Fatalf("Rebar Version: %s", rbversion.REBAR_VERSION)
	}
	if debug {
		jsonselect.EnableLogger()
	} else {
		gin.SetMode(gin.ReleaseMode)
	}
	log.Printf("Rebar Version: %s   Version: 0.2.1\n", rbversion.REBAR_VERSION)
	var port int
	if pStr := os.Getenv("APIPORT"); pStr == "" {
		log.Fatalln("APIPORT not set, no idea where we will listen")
	} else if port, err = strconv.Atoi(pStr); err != nil {
		log.Fatalf("APIPORT not an integer: %v", err)
	}
	cClient, err := client.Consul(true)
	if err != nil {
		log.Fatal(err.Error())
	}
	var bs store.SimpleStore
	switch backingStore {
	case "consul":
		bs, err = store.NewSimpleConsulStore(cClient, dataDir)
	case "file":
		bs, err = store.NewSimpleLocalStore(dataDir)
	default:
		log.Fatalf("Unknown backing store %s", backingStore)
	}
	if err != nil {
		log.Fatalf("Failed to create backing store %s at %s: %v", backingStore, dataDir, err)
	}
	rebarClient, err := client.Trusted("system", true)
	if err != nil {
		log.Fatalf("Error creating Rebar API client: %v", err)
	}

	ruleEngine, err = engine.NewEngine(bs, rebarClient, true, nil)
	if err != nil {
		log.Fatalf("Error creating rule engine: %v", err)
	}

	// Register service with Consul before continuing
	reg := &consul.AgentServiceRegistration{
		Name: "rule-engine-service",
		Tags: []string{"revproxy"}, // We want to be exposed through the revproxy
		Port: port,
		Check: &consul.AgentServiceCheck{
			Script:   "pidof rule-engine",
			Interval: "10s",
		},
	}
	if err = service.Register(cClient, reg, false); err != nil {
		log.Fatalf("Failed to register with Consul: %v", err)
	}

	// Now that we are all registered, register our actual listen address
	for {
		svc, err := service.Find(cClient, "rule-engine", "revproxy")
		if err != nil {
			log.Fatalf("Error talking to Consul: %v", err)
		}
		if len(svc) == 0 {
			continue
		}
		lAddr, lPort := service.Address(svc[0])
		listen = fmt.Sprintf("%s:%d", lAddr, lPort)
		break
	}

	ruleEngine.Debug = debug
	log.Printf("Talking to Rebar API at %s, our API at %s", rebarClient.URL, listen)

	// Create the capabilities we need.
	for _, capName := range []string{"RULESET_READ", "RULESET_UPDATE"} {
		cap := &api.Capability{}
		cap.Name = capName
		if err := rebarClient.Read(cap); err != nil {
			log.Printf("Creating capability %s", capName)
			cap.Description = "Allow access to actions on rule engine rulesets"
			cap.Source = "Rule Engine"
			cap.Name = capName
			if err := rebarClient.BaseCreate(cap); err != nil {
				log.Fatalf("Failed to create capability %s: %v", capName, err)
			}
		}
	}

	// Clean up on SIGTERM
	killChan := make(chan os.Signal, 1)
	go func() {
		<-killChan
		ruleEngine.Stop()
		os.Exit(0)
	}()
	signal.Notify(killChan, syscall.SIGTERM)
	signal.Notify(killChan, syscall.SIGINT)
	ruleEngine.RegisterSink(fmt.Sprintf("https://%s/events", listen))
	router.POST("/events", gin.WrapH(ruleEngine.Sink))
	apiv0 := router.Group("/api/v0")
	apiv0.Use(capMiddleware)
	apiv0.GET("/rulesets/", listRulesets)
	apiv0.GET("/rulesets/:name", showRuleset)
	apiv0.POST("/rulesets/", createRuleset)
	apiv0.PUT("/rulesets/:name", updateRuleset)
	apiv0.DELETE("/rulesets/:name", deleteRuleset)
	s, err := cert.Server("internal", "rule-engine-service")
	if err != nil {
		log.Fatalf("Failed to create trusted server: %v", err)
	}
	s.Addr = listen
	s.Handler = router

	log.Printf("Ready to handle events\n")
	for {
		log.Printf("API failed: %v", s.ListenAndServeTLS("", ""))
	}
}
