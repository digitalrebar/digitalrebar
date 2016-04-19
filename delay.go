package main

import (
	"log"
	"time"
)

type DeferredActions struct {
	duration int
	actions  []Action
	context  *RunContext
}

var delay_chan chan DeferredActions

func init() {
	delay_chan = make(chan DeferredActions, 100)
	start_delayer()
}

func delay_actions(e *RunContext, duration int, actions []Action) {
	log.Printf("Adding delay action for %v seconds with these actions %v", duration, actions)
	a := DeferredActions{
		duration: duration,
		actions:  actions,
		context:  e,
	}
	delay_chan <- a
}

func delaySecond(n int) {
	time.Sleep(time.Duration(n) * time.Second)
}

func run_actions(duration int, e *RunContext, actions []Action) {
	log.Printf("Waiting %d seconds for actions %v\n", duration, actions)
	delaySecond(duration)

	for _, action := range actions {
		err := action(e)
		if err != nil {
			log.Printf("Action failed: %v", err)
		}
	}

}

func start_delayer() {
	log.Printf("Delayer starting: %v", delay_chan)

	// Reader
	go func() {
		for {
			log.Printf("Wait for delay...\n")
			r := <-delay_chan
			log.Printf("received delay = %v\n", r)

			go run_actions(r.duration, r.context, r.actions)
		}
	}()

}
