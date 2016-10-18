package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes/provisioner"

type provisionerSrc struct{}

func (o *provisionerSrc) serviceSrc() string {
	return "provisioner-mgmt"
}

func (o *provisionerSrc) pathPrefix(trusted bool) string {
	if trusted {
		return ""
	}
	return "/provisioner"
}

type ProvisionerBootEnv struct {
	provisioner.BootEnv
	apiHelper
	provisionerSrc
}

type ProvisionerTemplate struct {
	provisioner.Template
	apiHelper
	provisionerSrc
}

type ProvisionerMachine struct {
	provisioner.Machine
	apiHelper
	provisionerSrc
}
