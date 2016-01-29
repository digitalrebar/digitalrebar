package main

import (
	"bytes"
	"fmt"
	"os"
	"path"
	"path/filepath"
	"text/template"
)

type renderData struct {
	Node *Node
	Env  *BootEnv
}

func parseTemplates(bootenv *BootEnv) error {
	for _, templateParams := range bootenv.Templates {
		pathTmpl, err := template.New(templateParams.Name).Parse(templateParams.Path)
		if err != nil {
			return fmt.Errorf("template: Error compiling path template %s (%s): %v",
				templateParams.Name,
				templateParams.Path,
				err)
		}
		tmpl, err := template.New(templateParams.Name).Parse(templateParams.Contents)
		if err != nil {
			return fmt.Errorf("template: Error compiling template %s: %v\n---template---\n %s",
				templateParams.Name,
				err,
				templateParams.Contents)
		}
		templateParams.pathTmpl = pathTmpl
		templateParams.contentTmpl = tmpl
	}
	return nil
}

func RenderTemplates(node *Node, bootenv *BootEnv) error {
	// nodeDir :=
	// uefiDir :=
	// pxeDir := path.Join(uefiDir, "pxelinux.cfg")
	vars := &renderData{
		Node: node,
		Env:  bootenv,
	}
	if err := parseTemplates(bootenv); err != nil {
		return err
	}

	for _, templateParams := range bootenv.Templates {
		pathBuf := &bytes.Buffer{}
		if err := templateParams.pathTmpl.Execute(pathBuf, vars); err != nil {
			return fmt.Errorf("template: Error rendering path %s (%s): %v",
				templateParams.Name,
				templateParams.Path,
				err)
		}
		tmplPath := filepath.Clean(filepath.Join(fileRoot, pathBuf.String()))
		if err := os.MkdirAll(path.Dir(tmplPath), 0755); err != nil {
			return fmt.Errorf("template: Unable to create dir for %s: %v", tmplPath, err)
		}

		tmplDest, err := os.Create(tmplPath)
		if err != nil {
			return fmt.Errorf("template: Unable to create file %s: %v", tmplPath, err)
		}
		defer tmplDest.Close()
		if err := templateParams.contentTmpl.Execute(tmplDest, vars); err != nil {
			os.Remove(tmplPath)
			return fmt.Errorf("template: Error rendering template %s: %v\n---template---\n %s",
				templateParams.Name,
				err,
				templateParams.Contents)
		}
		tmplDest.Sync()
	}
	return nil
}

func DeleteRenderedTemplates(node *Node, bootenv *BootEnv) {
	return
}
