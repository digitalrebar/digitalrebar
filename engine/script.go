package engine

/*
Copyright (c) 2016, Rackn Inc.
Licensed under the terms of the Digital Rebar License.
See LICENSE.md at the top of this repository for more information.
*/

import (
	"bytes"
	"fmt"
	"log"
	"os"
	"os/exec"
	"text/template"
)

func compileScript(script string) (*template.Template, error) {
	res := template.New("script").Option("missingkey=error")
	return res.Parse(script)
}

func runScript(e *RunContext, scriptTmpl *template.Template) (bool, error) {
	envVars := map[string]string{
		"REBAR_ENDPOINT": e.Engine.rebarEndpoint,
		"REBAR_KEY":      fmt.Sprintf("%s:%s", e.Engine.username, e.Engine.password),
	}
	buf := &bytes.Buffer{}
	if err := scriptTmpl.Execute(buf, e); err != nil {
		return false, err
	}

	cmd := exec.Command("/usr/bin/env", "bash", "-x")

	cmd.Env = os.Environ()
	for k, v := range envVars {
		cmd.Env = append(cmd.Env, fmt.Sprintf("%s=%s", k, v))
	}
	cmd.Stdin = buf
	out, err := cmd.Output()
	if err == nil {
		log.Printf("Ruleset %s: Script rule %d ran successfully", e.ruleset.Name, e.ruleIdx)
		log.Printf("%s", string(out))
		return true, nil
	}
	log.Printf("Ruleset %s: Script rule %d failed", e.ruleset.Name, e.ruleIdx)
	exitErr, ok := err.(*exec.ExitError)
	if ok {
		log.Printf("%s", string(exitErr.Stderr))
		return false, nil
	}
	log.Printf("Failed with error %v", err)
	return false, err
}
