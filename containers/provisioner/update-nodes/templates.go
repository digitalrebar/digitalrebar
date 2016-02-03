package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"path"
	"text/template"

	"github.com/labstack/echo"
)

// Template represents a template that will be associated with a boot environment.
type Template struct {
	UUID       string // UUID is a unique identifier for this template.
	Contents   string // Contents is the raw template.
	parsedTmpl *template.Template
}

func (t *Template) prefix() string {
	return "templates"
}

func (t *Template) key() string {
	return path.Join(t.prefix(), t.UUID)
}

// Parse checks to make sure the template contents are valid according to text/template.
func (t *Template) Parse() (err error) {
	parsedTmpl, err := template.New(t.UUID).Parse(t.Contents)
	if err != nil {
		return err
	}
	t.parsedTmpl = parsedTmpl.Option("missingkey=error")
	return nil
}

func createTemplate(c *echo.Context) error {
	finalStatus := http.StatusCreated
	thing := &Template{}
	thing.UUID = c.P(0)
	if err := backend.load(thing); err == nil {
		finalStatus = http.StatusAccepted
	}
	buf, err := ioutil.ReadAll(c.Request().Body)
	if err != nil {
		return fmt.Errorf("template: failed to read request body")
	}
	thing.Contents = string(buf)
	if err := backend.save(thing, nil); err != nil {
		return c.JSON(http.StatusInternalServerError, NewError(err.Error()))
	}
	return c.JSON(finalStatus, thing)
}

func (t *Template) onChange(oldThing interface{}) error {
	if t.Contents == "" || t.UUID == "" {
		return fmt.Errorf("template: Illegal template %+v", t)
	}
	if err := t.Parse(); err != nil {
		return fmt.Errorf("template: %s does not compile: %v", t.UUID, err)
	}
	if oldThing != nil {
		old := oldThing.(*Template)
		if old.UUID != t.UUID {
			return fmt.Errorf("template: Cannot change UUID of %s", t.UUID)
		}
	}
	return nil
}

func (t *Template) onDelete() error {
	bootenv := &BootEnv{}
	bootEnvs, err := bootenv.List()
	if err == nil {
		for _, bootEnv := range bootEnvs {
			for _, tmpl := range bootEnv.Templates {
				if tmpl.UUID == t.UUID {
					return fmt.Errorf("template: %s is in use by bootenv %s (template %s", t.UUID, bootEnv.Name, tmpl.Name)
				}
			}
		}
	}
	return err
}

// Render executes the template with params writing the results to dest
func (t *Template) Render(dest io.Writer, params interface{}) error {
	if t.parsedTmpl == nil {
		if err := t.Parse(); err != nil {
			return fmt.Errorf("template: %s does not compile: %v", t.UUID, err)
		}
	}
	if err := t.parsedTmpl.Execute(dest, params); err != nil {
		return fmt.Errorf("template: cannot execute %s: %v", t.UUID, err)
	}
	return nil
}
