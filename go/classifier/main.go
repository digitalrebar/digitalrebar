package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"github.com/ghodss/yaml"

	jsonselect "github.com/coddingtonbear/go-jsonselect"
	"github.com/digitalrebar/digitalrebar/go/common/store"
	"github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	"github.com/digitalrebar/digitalrebar/go/rule-engine/engine"
	"github.com/gin-gonic/gin"
)

var (
	debug      = false
	router     = gin.Default()
	listen     string
	ruleEngine *engine.Engine
	rulePath   string
)

func main() {
	var err error
	var endpoint string
	var username string
	var password string
	flag.StringVar(&rulePath, "rules", "classifier.yml", "Rules for this classifier to use.")
	flag.StringVar(&listen, "listen", ":35000", "Address:port to listen for events on.")
	flag.BoolVar(&debug, "debug", false, "Whether to run in debug mode")
	flag.Parse()

	if debug {
		jsonselect.EnableLogger()
	} else {
		gin.SetMode(gin.ReleaseMode)
	}
	// Get our username and password from the environment.
	if ep := os.Getenv("REBAR_ENDPOINT"); ep != "" {
		endpoint = ep
	} else {
		log.Fatalf("REBAR_ENDPOINT not set, no idea where Rebar is")
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
	} else {
		log.Fatalf("REBAR_KEY not set, will not be able to authenticate with Rebar")
	}
	if username == "" || password == "" {
		log.Fatalf("REBAR_KEY did not contain a useable username:password pair")
	}

	// Use the in-memory store.
	store := store.NewSimpleMemoryStore()

	// Get a connection to the Rebar API
	rebarClient, err := api.Session(endpoint, username, password)
	if err != nil {
		log.Fatalf("Error talking to Rebar: %v", err)
	}

	// Create a RuleEngine instance
	ruleEngine, err := engine.NewEngine(store, rebarClient, false, nil)
	if err != nil {
		log.Fatalf("Error creating rule engine: %v", err)
	}
	ruleEngine.Debug = debug
	// Handle cleanup on shutdown
	killChan := make(chan os.Signal, 1)
	go func() {
		<-killChan
		ruleEngine.Stop()
		os.Exit(0)
	}()
	signal.Notify(killChan, syscall.SIGTERM)
	signal.Notify(killChan, syscall.SIGINT)
	// Fire up the listener
	if err := ruleEngine.RegisterSink(fmt.Sprintf("http://%s/", listen)); err != nil {
		log.Fatalf("Failed to register event sink: %v", err)
	}
	router.POST("/", gin.WrapH(ruleEngine.Sink))
	go func() {
		err := router.Run(listen)
		if err != nil {
			log.Printf("Error handling event: %v", err)
			time.Sleep(1 * time.Second)
		}
	}()
	// Load initial ruleset
	rs := engine.RuleSet{}
	body, err := ioutil.ReadFile(rulePath)
	if err != nil {
		log.Fatalf("Error reading initial ruleset: %v", err)
	}
	if err := yaml.Unmarshal(body, &rs); err != nil {
		log.Fatalf("Invalid initial ruleset: %v", err)
	}
	rs.Active = true
	rs, err = ruleEngine.AddRuleSet(rs)
	if err != nil {
		log.Fatalf("Error using initial ruleset: %v", err)
	}

	// Reload rules on sighup forever.
	hupChan := make(chan os.Signal, 1)
	for {
		<-hupChan
		body, err := ioutil.ReadFile(rulePath)
		if err != nil {
			log.Printf("Error reading ruleset: %v", err)
			continue
		}
		if err := yaml.Unmarshal(body, &rs); err != nil {
			log.Printf("Invalid ruleset: %v", err)
			continue
		}
		rs, err = ruleEngine.UpdateRuleSet(rs)
		if err != nil {
			log.Printf("Error updating ruleset: %v", err)
		}
	}
}
