package main

import (
	"encoding/json"
	"fmt"
	"net"
	"path"
	"strings"
)

// Machine represents a single bare-metal system that the provisioner
// should manage the boot environment for.
type Machine struct {
	Name    string                 // The FQDN of the machine.
	Uuid    string                 // the UUID of the machine
	Address string                 // The IPv4 address that the machine PXE boots with.
	BootEnv string                 // The boot environment that the machine should boot into.
	Params  map[string]interface{} // Any additional parameters that may be needed for template expansion.
}

// HexAddress returns Address in raw hexadecimal format, suitable for
// pxelinux and elilo usage.
func (n *Machine) HexAddress() string {
	addr := net.ParseIP(n.Address).To4()
	hexIP := []byte(addr)
	return fmt.Sprintf("%02X%02X%02X%02X", hexIP[0], hexIP[1], hexIP[2], hexIP[3])
}

func (n *Machine) ShortName() string {
	idx := strings.Index(n.Name, ".")
	if idx == -1 {
		return n.Name
	}
	return n.Name[:idx]
}

func (n *Machine) UUID() string {
	if n.Uuid == "" {
		return n.Name
	}
	return n.Uuid
}

func (n *Machine) Url() string {
	return provisionerURL + "/" + n.key()
}

func (n *Machine) prefix() string {
	return "machines"
}

func (n *Machine) Path() string {
	return path.Join(n.prefix(), n.UUID())
}

func (n *Machine) key() string {
	return n.Path()
}

func (n *Machine) newIsh() keySaver {
	res := &Machine{Name: n.Name, Uuid: n.Uuid}
	return keySaver(res)
}

func (n *Machine) onChange(oldThing interface{}) error {
	if old, ok := oldThing.(*Machine); ok && old != nil {
		if old.Uuid != "" {
			if old.Uuid != n.Uuid {
				return fmt.Errorf("machine: Cannot change machine UUID %s", old.Uuid)
			}
		} else if old.Name != n.Name {
			return fmt.Errorf("machine: Cannot change name of machine %s", old.Name)
		}
		oldBootEnv := &BootEnv{Name: old.BootEnv}
		if err := backend.load(oldBootEnv); err != nil {
			return err
		}
		oldBootEnv.DeleteRenderedTemplates(old)
	}
	addr := net.ParseIP(n.Address)
	if addr != nil {
		addr = addr.To4()
	}
	if addr == nil {
		return fmt.Errorf("machine: %s  is not a valid IPv4 address", n.Address)
	}
	bootEnv := &BootEnv{Name: n.BootEnv}
	if err := backend.load(bootEnv); err != nil {
		return err
	}
	if err := bootEnv.RenderTemplates(n); err != nil {
		return err
	}
	return nil
}

func (n *Machine) onDelete() error {
	bootEnv := &BootEnv{Name: n.BootEnv}
	if err := backend.load(bootEnv); err != nil {
		return err
	}
	bootEnv.DeleteRenderedTemplates(n)
	return nil
}

func (b *Machine) List() ([]*Machine, error) {
	things := backend.list(b)
	res := make([]*Machine, len(things))
	for i, blob := range things {
		machine := &Machine{}
		if err := json.Unmarshal(blob, machine); err != nil {
			return nil, err
		}
		res[i] = machine
	}
	return res, nil
}

func (b *Machine) RebuildRebarData() error {
	return nil
}
