package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes/provisioner"

type ProvisionerBootEnv struct {
	provisioner.BootEnv
	apiHelper
}

type ProvisionerTemplate struct {
	provisioner.Template
	apiHelper
}

type ProvisionerMachine struct {
	provisioner.Machine
	apiHelper
}
