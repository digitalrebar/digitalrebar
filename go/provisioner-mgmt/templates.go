package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"strconv"
	"text/template"

	"github.com/digitalrebar/digitalrebar/go/common/multi-tenancy"
	"github.com/digitalrebar/digitalrebar/go/common/store"
	"github.com/gin-gonic/gin"
)

// Template represents a template that will be associated with a boot environment.
type Template struct {
	UUID       string // UUID is a unique identifier for this template.
	Contents   string // Contents is the raw template.
	parsedTmpl *template.Template
	TenantId   int
}

func (t *Template) Prefix() string {
	return "templates"
}

func (t *Template) Backend() store.SimpleStore {
	return getBackend(t)
}

func (t *Template) Key() string {
	return t.UUID
}

func (t *Template) tenantId() int {
	return t.TenantId
}

func (t *Template) setTenantId(tid int) {
	t.TenantId = tid
}

func (t *Template) typeName() string {
	return "TEMPLATE"
}

func (t *Template) New() store.KeySaver {
	res := &Template{UUID: t.UUID}
	return store.KeySaver(res)
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
	tenant_id, err := strconv.Atoi(c.Query(`tenant_id`))
	if err != nil {
		tenant_id = 1
	}
	capMap, err := multitenancy.NewCapabilityMap(c.Request)
	if err != nil {
		c.JSON(http.StatusInternalServerError,
			NewError(fmt.Sprintf("list: failed to get the capmap: %v", err)))
		return
	}
	oldThing := &Template{UUID: c.Param(`uuid`), TenantId: tenant_id}
	newThing := &Template{UUID: c.Param(`uuid`), TenantId: tenant_id}
	var action string
	if found, _ := store.Load(oldThing); !found {
		finalStatus = http.StatusAccepted
		action = "UPDATE"
	} else {
		oldThing = nil
		action = "CREATE"
	}
	buf, err := ioutil.ReadAll(c.Request.Body)
	if err != nil {
		c.Error(fmt.Errorf("template: failed to read request body"))
		c.Data(http.StatusExpectationFailed, gin.MIMEJSON, nil)
	}
	newThing.Contents = string(buf)
	if oldThing != nil && !capMap.HasCapability(oldThing.tenantId(), "TEMPLATE_"+action) {
		c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
		return
	}
	if !capMap.HasCapability(newThing.tenantId(), "TEMPLATE_"+action) {
		c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
		return
	}
	if saved, err := store.Update(newThing); !saved {
		c.JSON(http.StatusInternalServerError, NewError(err.Error()))
	}
	c.JSON(finalStatus, newThing)
}

func (t *Template) BeforeCreate() error {
	return t.OnChange(nil)
}

func (t *Template) OnChange(oldThing store.KeySaver) error {
	if t.Contents == "" || t.UUID == "" {
		return fmt.Errorf("template: Illegal template %+v", t)
	}
	if err := t.Parse(); err != nil {
		return fmt.Errorf("template: %s does not compile: %v", t.UUID, err)
	}

	if old, ok := oldThing.(*Template); ok && old != nil && old.UUID != t.UUID {
		return fmt.Errorf("template: Cannot change UUID of %s", t.UUID)
	}
	machine := &Machine{}
	machines, err := machine.List()
	if err == nil {
		for _, machine := range machines {
			reRender := false
			bootEnv := &BootEnv{Name: machine.BootEnv}
			if found, _ := store.Load(bootEnv); found {
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
	return nil
}

func (t *Template) BeforeDelete() error {
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
