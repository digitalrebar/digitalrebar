package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"net"
)

type Node struct {
	Name       string
	Address    string
	HexAddress string
	BootEnv    string
	Params     map[string]string
}

func (n *Node) Prefix() string {
	return nodeKey + "/nodes"
}

func (n *Node) Key() string {
	return n.Prefix() + "/" + n.Name
}

func (n *Node) OnChange(oldThing interface{}) error {
	if oldThing != nil {
		old := oldThing.(*Node)
		if old.Name != n.Name {
			return errors.New("Cannot change name of node")
		}
		oldBootEnv := &BootEnv{Name: old.BootEnv}
		if err := backend.Load(oldBootEnv); err != nil {
			return err
		}
		DeleteRenderedTemplates(old, oldBootEnv)
	}
	addr := net.ParseIP(n.Address)
	if addr != nil {
		addr = addr.To4()
	}
	if addr == nil {
		return errors.New(n.Address + " is not a valid IPv4 address")
	}
	hexIP := []byte(addr)
	n.HexAddress = fmt.Sprintf("%02X%02X%02X%02X", hexIP[0], hexIP[1], hexIP[2], hexIP[3])
	bootEnv := &BootEnv{Name: n.BootEnv}
	if err := backend.Load(bootEnv); err != nil {
		return err
	}
	if err := RenderTemplates(n, bootEnv); err != nil {
		return err
	}
	return nil
}

func (n *Node) OnDelete() error {
	bootEnv := &BootEnv{Name: n.BootEnv}
	if backend.Load(bootEnv) != nil {
		DeleteRenderedTemplates(n, bootEnv)
	}
	return nil
}

func (b *Node) List() ([]*Node, error) {
	things := backend.List(b)
	res := make([]*Node, len(things))
	for i, blob := range things {
		node := &Node{}
		if err := json.Unmarshal(blob, node); err != nil {
			return nil, err
		}
		res[i] = node
	}
	return res, nil
}
