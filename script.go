package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"
)

func runScript(e *RunContext, script string) (bool, error) {
	buf, err := json.Marshal(e)
	if err != nil {
		log.Panicf("Unable to marshal Event for a script run!")
	}
	envVars := map[string]string{
		"CLASSIFIER_CONTEXT": string(buf),
		"REBAR_ENDPOINT":     endpoint,
		"REBAR_KEY":          fmt.Sprintf("%s:%s", username, password),
	}

	cmd := exec.Command("/usr/bin/env", "bash", "-x")

	cmd.Env = os.Environ()
	for k, v := range envVars {
		cmd.Env = append(cmd.Env, fmt.Sprintf("%s=%s", k, v))
	}
	cmd.Stdin = strings.NewReader(script)
	out, err := cmd.Output()
	if err == nil {
		log.Printf("Script rule %s ran successfully", e.rule.Name)
		log.Printf("%s", string(out))
		return true, nil
	} else {
		log.Printf("Script rule %s failed", e.rule.Name)
		exitErr, ok := err.(*exec.ExitError)
		if ok {
			log.Printf("%s", string(exitErr.Stderr))
			return false, nil
		} else {
			log.Printf("Failed with error %v", err)
			return false, err
		}
	}
}
