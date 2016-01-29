package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"text/template"
)

type TemplateInfo struct {
	Name        string
	Path        string
	Contents    string
	pathTmpl    *template.Template
	contentTmpl *template.Template
}

type OsInfo struct {
	Name      string
	Family    string
	Codename  string
	Version   string
	IsoFile   string
	IsoSha256 string
	IsoUrl    string
}

type BootEnv struct {
	Name         string
	OS           OsInfo
	Templates    []*TemplateInfo
	Kernel       string
	Initrds      []string
	BootParams   []string
	Repositories []string
}

func (b *BootEnv) Prefix() string {
	return "/bootenvs"
}

func (b *BootEnv) Key() string {
	return b.Prefix() + "/" + b.Name
}

func (b *BootEnv) OnChange(oldThing interface{}) error {
	seenPxeLinux := false
	seenELilo := false
	for _, template := range b.Templates {
		if template.Name == "pxelinux" {
			seenPxeLinux = true
		}
		if template.Name == "elilo" {
			seenELilo = true
		}
		if template.Name == "" ||
			template.Path == "" ||
			template.Contents == "" {
			return errors.New(fmt.Sprintf("Illegal template: %+v", template))
		}
	}
	if !(seenPxeLinux && seenELilo) {
		return errors.New("Missing elilo or pxelinux template")
	}
	if err := parseTemplates(b); err != nil {
		return err
	}
	if oldThing != nil {
		old := oldThing.(*BootEnv)
		if old.Name != b.Name {
			return errors.New("Cannot change name of bootenv")
		}
		node := &Node{}
		nodes, err := node.List()
		if err != nil {
			return err
		}

		for _, node := range nodes {
			if node.BootEnv != old.Name {
				continue
			}
			if err := RenderTemplates(node, b); err != nil {
				return err
			}
		}
	}
	return nil
}

func (b *BootEnv) OnDelete() error {
	node := &Node{}
	nodes, err := node.List()
	if err == nil {
		for _, node := range nodes {
			if node.BootEnv != b.Name {
				continue
			}
			return errors.New(fmt.Sprintf("Bootenv %s in use by Node %s", b.Name, node.Name))
		}
	}
	return nil
}

func (b *BootEnv) List() ([]*BootEnv, error) {
	things := backend.List(b)
	res := make([]*BootEnv, len(things))
	for i, blob := range things {
		bootenv := &BootEnv{}
		if err := json.Unmarshal(blob, bootenv); err != nil {
			return nil, err
		}
		res[i] = bootenv
	}
	return res, nil
}
