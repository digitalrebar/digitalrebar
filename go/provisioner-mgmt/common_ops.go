package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strconv"

	"github.com/VictorLowther/jsonpatch"
	"github.com/digitalrebar/digitalrebar/go/common/multi-tenancy"
	"github.com/digitalrebar/digitalrebar/go/common/store"
	"github.com/gin-gonic/gin"
)

func testCap(c *gin.Context, tenant int, op string) bool {
	cap, capOK := c.Get("Capabilities")
	if !capOK {
		return false
	}
	capSet, ok := cap.(multitenancy.CapabilityMap)
	if !ok {
		return false
	}
	return capSet.HasCapability(tenant, op)
}

func listThings(c *gin.Context, thing keySaver) {
	objType := thing.typeName()
	things := [][]byte{}
	var err error
	things, err = store.ListRaw(thing.Backend())
	if err != nil {
		things = [][]byte{}
	}
	res := make([]interface{}, 0, len(things))
	for _, obj := range things {
		var buf interface{}
		if err := json.Unmarshal(obj, &buf); err != nil {
			c.JSON(http.StatusInternalServerError,
				NewError(fmt.Sprintf("list: error unmarshalling %v: %v", string(obj), err)))
			return
		}
		ent, ok := buf.(map[string]interface{})
		if !ok {
			c.JSON(http.StatusInternalServerError,
				NewError(fmt.Sprintf("list: error casting %v: %v", string(obj), ok)))
			return
		}
		ti, ok := ent["TenantId"]
		if !ok {
			c.JSON(http.StatusInternalServerError,
				NewError(fmt.Sprintf("list: failed to find ti %v: %v", ent, ok)))
			return
		}

		tf, ok := ti.(float64)
		if !ok {
			c.JSON(http.StatusInternalServerError,
				NewError(fmt.Sprintf("list: failed to cast tf %v: %v", tf, ok)))
			return
		}

		if testCap(c, int(tf), objType+"_READ") {
			res = append(res, buf)
		}
	}
	c.JSON(http.StatusOK, res)
}

func createThing(c *gin.Context, newThing keySaver) {
	objType := newThing.typeName()
	var err error
	if err = c.Bind(&newThing); err != nil {
		c.JSON(http.StatusBadRequest, NewError(err.Error()))
		return
	}

	stid := c.Query("tenant_id")
	if stid != "" {
		tid, err := strconv.Atoi(stid)
		if err != nil {
			c.JSON(http.StatusBadRequest, NewError(err.Error()))
			return
		}
		newThing.setTenantId(tid)
	}

	finalStatus := http.StatusCreated
	oldThing := newThing.New().(keySaver)
	saved := false
	if ok, _ := store.Load(oldThing); ok {
		logger.Printf("backend: Updating %v %d\n", oldThing.Key(), oldThing.tenantId())
		logger.Printf("backend: Updating new %v %d\n", newThing.Key(), newThing.tenantId())
		if !testCap(c, oldThing.tenantId(), objType+"_UPDATE") {
			c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
			return
		}
		if !testCap(c, newThing.tenantId(), objType+"_UPDATE") {
			c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
			return
		}
		finalStatus = http.StatusAccepted
		saved, err = store.Update(newThing)
	} else {
		logger.Printf("backend: Creating %v %d\n", newThing.Key(), newThing.tenantId())
		if !testCap(c, newThing.tenantId(), objType+"_CREATE") {
			c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
			return
		}
		oldThing = nil
		saved, err = store.Create(newThing)
	}
	if !saved {
		logger.Printf("backend: Save failed: %v\n", err)
		c.JSON(http.StatusConflict, NewError(err.Error()))
		return
	}
	c.JSON(finalStatus, newThing)
}

func getThing(c *gin.Context, thing keySaver) {
	objType := thing.typeName()

	if found, _ := store.Load(thing); !found {
		c.Data(http.StatusNotFound, gin.MIMEJSON, nil)
		return
	}
	if !testCap(c, thing.tenantId(), objType+"_READ") {
		c.Data(http.StatusNotFound, gin.MIMEJSON, nil)
		return
	}

	c.JSON(http.StatusOK, thing)
}

func updateThing(c *gin.Context, oldThing, newThing keySaver) {
	objType := newThing.typeName()

	if found, _ := store.Load(oldThing); !found {
		c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
		return
	}
	if !testCap(c, oldThing.tenantId(), objType+"_UPDATE") {
		c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
		return
	}

	stid := c.Query("tenant_id")
	if stid != "" {
		tid, err := strconv.Atoi(stid)
		if err != nil {
			c.JSON(http.StatusBadRequest, NewError(err.Error()))
			return
		}
		oldThing.setTenantId(tid)
	}

	patch, err := ioutil.ReadAll(c.Request.Body)
	if err != nil {
		c.Error(err)
		c.Data(http.StatusExpectationFailed, gin.MIMEJSON, nil)
		return
	}
	oldThingBuf, _ := json.Marshal(oldThing)
	newThingBuf, err, loc := jsonpatch.ApplyJSON(oldThingBuf, patch)
	if err != nil {
		c.JSON(http.StatusConflict, NewError(fmt.Sprintf("Failed to apply patch at %d: %v\n", loc, err)))
		return
	}
	if err := json.Unmarshal(newThingBuf, &newThing); err != nil {
		c.Error(err)
		c.Data(http.StatusExpectationFailed, gin.MIMEJSON, nil)
		return
	}

	if !testCap(c, newThing.tenantId(), objType+"_UPDATE") {
		c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
		return
	}

	if saved, err := store.Update(newThing); !saved {
		c.JSON(http.StatusConflict, NewError(err.Error()))
		return
	}
	c.JSON(http.StatusAccepted, newThing)
}

func deleteThing(c *gin.Context, thing keySaver) {
	objType := thing.typeName()
	if found, _ := store.Load(thing); found {
		c.Data(http.StatusConflict, gin.MIMEJSON, nil)
		return
	}
	if !testCap(c, thing.tenantId(), objType+"_DESTROY") {
		c.Data(http.StatusConflict, gin.MIMEJSON, nil)
		return
	}
	if deleted, err := store.Remove(thing); !deleted {
		c.JSON(http.StatusConflict, NewError(fmt.Sprintf("Failed to delete %s: %v", thing.Key(), err)))
		return
	}
	c.Data(http.StatusAccepted, gin.MIMEJSON, nil)
}
