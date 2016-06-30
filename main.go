package main

/*
Copyright (c) 2016, Rackn Inc.
Licensed under the terms of the Digital Rebar License.
See LICENSE.md at the top of this repository for more information.
*/

import (
	"flag"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"github.com/ghodss/yaml"

	"github.com/coddingtonbear/go-jsonselect"
	"github.com/digitalrebar/rebar-api/client"
	"github.com/rackn/classifier/engine"
)

var (
	version                      = false
	debug                        = false
	testRules                    = false
	username, password, endpoint string
	listen                       string
	rulesetFile                  string
	ruleEngine                   *engine.Engine
)

func serve() {
	http.Handle("/", ruleEngine)
	for {
		log.Println(http.ListenAndServe(listen, nil))
	}
}

func main() {
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
	flag.StringVar(&rulesetFile, "rules", "", "File to read rules from")
	flag.StringVar(&username, "username", "", "Username for Digital Rebar endpoint")
	flag.StringVar(&password, "password", "", "Password for Digital Rebar endpoint")
	flag.StringVar(&endpoint, "endpoint", "", "API Endpoint for Digital Rebar")
	flag.StringVar(&listen, "listen", "", "Address to listen on for postbacks from Digital Rebar")
	flag.BoolVar(&version, "version", false, "Print version and exit")
	flag.BoolVar(&debug, "debug", false, "Whether to run in debug mode")
	flag.BoolVar(&testRules, "testRules", false, "Exit after validating that the rules can be loaded")
	flag.Parse()
	if version {
		log.Fatalf("Version: 0.2.1")
	}
	if debug {
		jsonselect.EnableLogger()
	}
	if rulesetFile == "" {
		log.Fatalf("You must load rules with --rules rulesetFile")
	}

	rsBuf, err := ioutil.ReadFile(rulesetFile)
	if err != nil {
		log.Fatalf("Failed to load rules from %s:\n%v", rulesetFile, err)
	}
	theRules := engine.RuleSet{}
	if err := yaml.Unmarshal(rsBuf, &theRules); err != nil {
		log.Fatalln(err)
	}

	if testRules {
		ruleEngine, _ = engine.NewEngine(listen, endpoint, username, password)
		ruleEngine.Debug = debug
		ruleEngine.Lock()
		if err := ruleEngine.AddRuleSet(theRules); err != nil {
			ruleEngine.DeleteRuleSet(theRules.Name)
			ruleEngine.Unlock()
			log.Fatalln(err)
		}
		os.Exit(0)
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
	if err := client.Session(endpoint, username, password); err != nil {
		log.Fatalf("Could not connect to Rebar: %v", err)
	}
	ruleEngine, err = engine.NewEngine(listen, endpoint, username, password)
	if err != nil {
		log.Fatalf("Error creating rule engine: %v", err)
	}
	ruleEngine.Debug = debug
	ruleEngine.Lock()
	if err := ruleEngine.AddRuleSet(theRules); err != nil {
		ruleEngine.DeleteRuleSet(theRules.Name)
		ruleEngine.Unlock()
		log.Fatalln(err)
	}
	ruleEngine.Unlock()
	// Run the handler loop
	go serve()

	// Clean up on SIGTERM
	killChan := make(chan os.Signal, 1)
	go func() {
		<-killChan
		ruleEngine.Stop()
		os.Exit(0)
	}()
	signal.Notify(killChan, syscall.SIGTERM)
	signal.Notify(killChan, syscall.SIGINT)

	// Reload rules file on SIGHUP
	hupChan := make(chan os.Signal, 1)
	go func() {
		for {
			<-hupChan
			rsBuf, err := ioutil.ReadFile(rulesetFile)
			if err != nil {
				log.Fatalf("Failed to load rules from %s:\n%v", rulesetFile, err)
			}
			theRules := engine.RuleSet{}
			if err := yaml.Unmarshal(rsBuf, &theRules); err != nil {
				log.Println(err)
			} else {
				ruleEngine.Lock()
				if err := ruleEngine.UpdateRuleSet(theRules); err != nil {
					log.Println(err)
				}
				ruleEngine.Unlock()
			}
		}
	}()
	signal.Notify(hupChan, syscall.SIGHUP)

	// Wait forever
	log.Printf("Ready to handle events\n")
	for {
		time.Sleep(100 * time.Second)
	}
}
