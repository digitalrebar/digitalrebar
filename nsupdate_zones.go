package main

import (
	"bufio"
	"io/ioutil"
	"os"
	"os/exec"
)

type NsupdateDnsInstance struct {
	dns_backend_point
	Server string
}

func NewNsupdateDnsInstance(serverName string) *NsupdateDnsInstance {
	return &NsupdateDnsInstance{
		Server: serverName,
	}
}

// List function
func (di *NsupdateDnsInstance) GetAllZones(zones *ZoneTracker) ([]Zone, *backendError) {
	answer := make([]Zone, 0, 10)
	for k, v := range zones.Zones {
		answer = append(answer, buildZone(k, v))
	}

	return answer, nil
}

// Get function
func (di *NsupdateDnsInstance) GetZone(zones *ZoneTracker, id string) (Zone, *backendError) {
	zdata := zones.Zones[id]
	if zdata == nil {
		return Zone{}, &backendError{"Not Found", 404}
	}

	return buildZone(id, zdata), nil
}

// Patch function
func (di *NsupdateDnsInstance) PatchZone(zones *ZoneTracker, zoneName string, rec Record) (Zone, *backendError) {
	var command string

	switch rec.ChangeType {
	case "ADD":
		command = "add"
	case "REMOVE":
		command = "delete"
	default:
		return Zone{}, &backendError{"Invalid Change Type", 400}
	}

	f, err := ioutil.TempFile("/tmp", "dnsupdate")
	if err != nil {
		return Zone{}, &backendError{"Server Error: " + err.Error(), 400}
	}
	defer f.Close()
	defer os.Remove(f.Name())

	w := bufio.NewWriter(f)

	w.WriteString("server " + di.Server + "\n")
	w.WriteString("zone " + zoneName + "\n")
	w.WriteString("update " + command + " " + rec.Name + "3600 IN " + rec.Type + " " + rec.Content + "\n")
	w.WriteString("show\nsend\nquit\n")
	w.Flush()

	cmdName := "nsupdate"
	cmdArgs := []string{f.Name()}
	if _, err = exec.Command(cmdName, cmdArgs...).Output(); err != nil {
		return Zone{}, &backendError{"Nsupdate command failed: " + err.Error(), 400}
	}

	return buildZone(zoneName, zones.Zones[zoneName]), nil
}
