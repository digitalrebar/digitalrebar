package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"path"
	"text/template"

	"github.com/gin-gonic/gin"
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

func (t *Template) newIsh() keySaver {
	res := &Template{UUID: t.UUID}
	return keySaver(res)
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

func createTemplate(c *gin.Context) {
	finalStatus := http.StatusCreated
	oldThing := &Template{UUID: c.Param(`uuid`)}
	newThing := &Template{UUID: c.Param(`uuid`)}
	if err := backend.load(oldThing); err == nil {
		finalStatus = http.StatusAccepted
	} else {
		oldThing = nil
	}
	buf, err := ioutil.ReadAll(c.Request.Body)
	if err != nil {
		c.Error(fmt.Errorf("template: failed to read request body"))
		c.Data(http.StatusExpectationFailed, gin.MIMEJSON, nil)
	}
	newThing.Contents = string(buf)
	if err := backend.save(newThing, oldThing); err != nil {
		c.JSON(http.StatusInternalServerError, NewError(err.Error()))
	}
	c.JSON(finalStatus, newThing)
}

func (t *Template) onChange(oldThing interface{}) error {
	if t.Contents == "" || t.UUID == "" {
		return fmt.Errorf("template: Illegal template %+v", t)
	}
	if err := t.Parse(); err != nil {
		return fmt.Errorf("template: %s does not compile: %v", t.UUID, err)
	}

	if old, ok := oldThing.(*Template); ok && old != nil && old.UUID != t.UUID {
		return fmt.Errorf("template: Cannot change UUID of %s", t.UUID)
		machine := &Machine{}
		machines, err := machine.List()
		if err == nil {
			for _, machine := range machines {
				reRender := false
				bootEnv := &BootEnv{Name: machine.BootEnv}
				if err := backend.load(bootEnv); err == nil {
					for _, template := range bootEnv.Templates {
						if template.UUID == t.UUID {
							reRender = true
							template.contents = t
							break
						}
					}
				}
				if reRender {
					bootEnv.RenderTemplates(machine)
				}
			}
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
