package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/digitalrebar/rebar-api/client"
)

var (
	debug                        = false
	username, password, endpoint string
	listen                       string
	rules                        []*Rule
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
	flag.StringVar(&username, "username", "", "Username for Digital Rebar endpoint")
	flag.StringVar(&password, "password", "", "Password for Digital Rebar endpoint")
	flag.StringVar(&endpoint, "endpoint", "", "API Endpoint for Digital Rebar")
	flag.StringVar(&listen, "listen", "", "Address to listen on for postbacks from Digital Rebar")
	flag.BoolVar(&debug, "debug", false, "Whether to run in debug mode")
	rules = append(rules, &Rule{
		Name:    "test logging rule",
		Actions: []Action{&LogAction{}},
	})
}

func handler(w http.ResponseWriter, req *http.Request) {
	log.Printf("Got request from %v", req.RemoteAddr)
	event := &Event{}
	if err := json.NewDecoder(req.Body).Decode(event); err != nil {
		log.Printf("Error decoding body: %v", err)
		w.WriteHeader(http.StatusExpectationFailed)
		return
	}
	RunRules(rules, event)
	w.WriteHeader(http.StatusAccepted)
}

func serve() {
	http.HandleFunc("/", handler)
	for {
		log.Println(http.ListenAndServe(listen, nil))
	}
}

func main() {
	flag.Parse()
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
	// Run the handler loop
	go serve()

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
	selector := make(map[string]interface{})
	selector["event"] = "on_milestone"
	selectorMatch := make(map[string]interface{})
	selectorMatch["event_sink_id"] = eventSink.ID
	selectorMatch["selector"] = selector
	eventSelector := &client.EventSelector{}
	eventSelectorMatches := make([]*client.EventSelector, 0)
	if err := client.Match(eventSelector.ApiName(), selectorMatch, &eventSelectorMatches); err != nil {
		log.Fatalf("Error getting event selectors: %v", err)
	}
	if len(eventSelectorMatches) > 0 {
		log.Printf("Event selector already present, moving on")
		eventSelector = eventSelectorMatches[0]
	} else {
		eventSelector.EventSinkID = eventSink.ID
		eventSelector.Selector = selector
		log.Printf("Creating new event selector")
		if err := client.BaseCreate(eventSelector); err != nil {
			log.Fatalf("Error creating event selector: %v", err)
		}
	}
	// Wait forever
	for {
		time.Sleep(100 * time.Second)
	}
}
