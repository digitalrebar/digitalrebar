package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"reflect"
	"strings"
	"syscall"
	"time"

	"github.com/coddingtonbear/go-jsonselect"
	"github.com/digitalrebar/rebar-api/client"
)

var (
	version                      = false
	debug                        = false
	testRules                    = false
	username, password, endpoint string
	listen                       string
	ruleFile                     string
	jsonSelectorList             string
	defaultJSONSelectorList      []map[string]interface{}
)

func init() {
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
	flag.StringVar(&ruleFile, "rules", "", "File to read rules from")
	flag.StringVar(&username, "username", "", "Username for Digital Rebar endpoint")
	flag.StringVar(&password, "password", "", "Password for Digital Rebar endpoint")
	flag.StringVar(&endpoint, "endpoint", "", "API Endpoint for Digital Rebar")
	flag.StringVar(&listen, "listen", "", "Address to listen on for postbacks from Digital Rebar")
	flag.BoolVar(&version, "version", false, "Print version and exit")
	flag.BoolVar(&debug, "debug", false, "Whether to run in debug mode")
	flag.BoolVar(&testRules, "testRules", false, "Exit after validating that the rules can be loaded")
	flag.StringVar(&jsonSelectorList, "jsonSelectorList", "[ { \"event\": \"on_milestone\" } ]", "Default selector list for rules without EventTypes")
}

func handler(w http.ResponseWriter, req *http.Request) {
	log.Printf("Got request from %v", req.RemoteAddr)
	event := &Event{}
	if err := json.NewDecoder(req.Body).Decode(event); err != nil {
		log.Printf("Error decoding body: %v", err)
		w.WriteHeader(http.StatusExpectationFailed)
		return
	}
	ctx := makeContext(event)
	ctx.Process(rules)
	w.WriteHeader(http.StatusAccepted)
}

func serve() {
	http.HandleFunc("/", handler)
	for {
		log.Println(http.ListenAndServe(listen, nil))
	}
}

func updateSinks() {
	// Register us as an event sink, if not already registered.
	finalEndpoint := fmt.Sprintf("http://%s/", listen)
	eventSink := &client.EventSink{}
	eventSinkMatch := make(map[string]interface{})
	eventSinkMatch["endpoint"] = finalEndpoint
	eventSinkMatches := make([]*client.EventSink, 0)
	if err := client.Match(eventSink.ApiName(), eventSinkMatch, &eventSinkMatches); err != nil {
		log.Fatalf("Error getting event sinks: %v", err)
	}
	if len(eventSinkMatches) > 0 {
		eventSink = eventSinkMatches[0]
		log.Printf("Event sink already present, moving on")
	} else {
		eventSink.Endpoint = finalEndpoint
		log.Printf("Creating new event sink")
		if err := client.BaseCreate(eventSink); err != nil {
			log.Fatalf("Error creating event sink: %v", err)
		}
	}

	// Get all of my selectors
	selectorMatch := make(map[string]interface{})
	selectorMatch["event_sink_id"] = eventSink.ID
	eventSelector := &client.EventSelector{}
	eventSelectorMatches := make([]*client.EventSelector, 0)
	if err := client.Match(eventSelector.ApiName(), selectorMatch, &eventSelectorMatches); err != nil {
		log.Fatalf("Error getting event selectors: %v", err)
	}

	// Get list of to make selectors
	selectors := eventTypes

	// Build lists of add, delete, already_present
	add := make([]map[string]interface{}, 0, 0)
	delete := make([]*client.EventSelector, 0, 0)
	already_present := make([]*client.EventSelector, 0, 0)

	// Negative Intersection new with old to get adds
	for _, v := range selectors {
		// v is selector struct

		// Test to see if this selector is in our list.
		doit := true
		for _, e := range eventSelectorMatches {
			if reflect.DeepEqual(e.Selector, v) {
				doit = false
				already_present = append(already_present, e)
				break
			}
		}
		if doit {
			add = append(add, v)
		}
	}

	// Negative Intersection old with new to get deletes
	for _, v := range eventSelectorMatches {
		found := false
		for _, e := range already_present {
			if v == e {
				found = true
				break
			}
		}

		if !found {
			delete = append(delete, v)
		}
	}

	// Add new selectors
	for _, v := range add {
		eventSelector = &client.EventSelector{}
		eventSelector.EventSinkID = eventSink.ID
		eventSelector.Selector = v
		log.Printf("Creating new event selector, %v", v)
		if err := client.BaseCreate(eventSelector); err != nil {
			log.Fatalf("Error creating event selector: %v", err)
		}
	}

	// Delete old selectors
	for _, v := range delete {
		log.Printf("Deleting old event selector, %v", v)
		if err := client.Destroy(v); err != nil {
			log.Fatalf("Error destroying event selector: %v", err)
		}
	}
}

func validateDefaultSelector() error {
	return json.Unmarshal([]byte(jsonSelectorList), &defaultJSONSelectorList)
}

func main() {
	flag.Parse()
	if version {
		log.Fatalf("Version: 0.2.1")
	}
	if debug {
		jsonselect.EnableLogger()
	}
	if ruleFile == "" {
		log.Fatalf("You must load rules with --rules ruleFile")
	}
	if err := validateDefaultSelector(); err != nil {
		log.Fatalf("Default Selector is invalid: %s\n%v", jsonSelectorList, err)
	}
	if err := loadRules(ruleFile); err != nil {
		log.Fatalf("Failed to load rules from %s:\n%v", ruleFile, err)
	}
	if testRules {
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

	start_delayer()

	// Run the handler loop
	go serve()

	// Update the sinks for the first time.
	updateSinks()

	// Reload rules file on SIGHUP
	hupChan := make(chan os.Signal, 1)
	go func() {
		for {
			<-hupChan
			if err := loadRules(ruleFile); err != nil {
				log.Printf("Error loading rules from %s: %v", ruleFile, err)
			} else {
				updateSinks()
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
