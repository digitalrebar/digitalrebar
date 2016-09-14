package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strconv"

	"github.com/VictorLowther/jsonpatch"
	"github.com/digitalrebar/digitalrebar/go/common/multi-tenancy"
	"github.com/gin-gonic/gin"
)

func listThings(c *gin.Context, thing keySaver) {
	objType := thing.typeName()

	capMap, err := multitenancy.NewCapabilityMap(c.Request)
	if err != nil {
		c.JSON(http.StatusInternalServerError,
			NewError(fmt.Sprintf("list: failed to get the capmap: %v", err)))
		return
	}

	things := backend.list(thing)
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

		if capMap.HasCapability(int(tf), objType+"_READ") {
			res = append(res, buf)
		}
	}
	c.JSON(http.StatusOK, res)
}

func createThing(c *gin.Context, newThing keySaver) {
	objType := newThing.typeName()

	if err := c.Bind(&newThing); err != nil {
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

	capMap, err := multitenancy.NewCapabilityMap(c.Request)
	if err != nil {
		c.JSON(http.StatusInternalServerError,
			NewError(fmt.Sprintf("list: failed to get the capmap: %v", err)))
		return
	}
	finalStatus := http.StatusCreated
	oldThing := newThing.newIsh()
	if err := backend.load(oldThing); err == nil {
		logger.Printf("backend: Updating %v %d\n", oldThing.key(), oldThing.tenantId())
		logger.Printf("backend: Updating new %v %d\n", newThing.key(), newThing.tenantId())
		if !capMap.HasCapability(oldThing.tenantId(), objType+"_UPDATE") {
			c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
			return
		}
		if !capMap.HasCapability(newThing.tenantId(), objType+"_UPDATE") {
			c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
			return
		}
		finalStatus = http.StatusAccepted
	} else {
		logger.Printf("backend: Creating %v %d\n", newThing.key(), newThing.tenantId())
		if !capMap.HasCapability(newThing.tenantId(), objType+"_CREATE") {
			c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
			return
		}
		oldThing = nil
	}
	if err := backend.save(newThing, oldThing); err != nil {
		logger.Printf("backend: Save failed: %v\n", err)
		c.JSON(http.StatusConflict, NewError(err.Error()))
		return
	}
	c.JSON(finalStatus, newThing)
}

func getThing(c *gin.Context, thing keySaver) {
	objType := thing.typeName()

	if err := backend.load(thing); err != nil {
		c.Data(http.StatusNotFound, gin.MIMEJSON, nil)
		return
	}
	capMap, err := multitenancy.NewCapabilityMap(c.Request)
	if err != nil {
		c.JSON(http.StatusInternalServerError,
			NewError(fmt.Sprintf("list: failed to get the capmap: %v", err)))
		return
	}
	if !capMap.HasCapability(thing.tenantId(), objType+"_READ") {
		c.Data(http.StatusNotFound, gin.MIMEJSON, nil)
		return
	}

	c.JSON(http.StatusOK, thing)
}

func updateThing(c *gin.Context, oldThing, newThing keySaver) {
	objType := newThing.typeName()

	capMap, err := multitenancy.NewCapabilityMap(c.Request)
	if err != nil {
		c.JSON(http.StatusInternalServerError,
			NewError(fmt.Sprintf("list: failed to get the capmap: %v", err)))
		return
	}
	if err := backend.load(oldThing); err != nil {
		c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
		return
	}
	if !capMap.HasCapability(oldThing.tenantId(), objType+"_UPDATE") {
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

	if !capMap.HasCapability(newThing.tenantId(), objType+"_UPDATE") {
		c.Data(http.StatusForbidden, gin.MIMEJSON, nil)
		return
	}

	if err := backend.save(newThing, oldThing); err != nil {
		c.JSON(http.StatusConflict, NewError(err.Error()))
		return
	}
	c.JSON(http.StatusAccepted, newThing)
}

func deleteThing(c *gin.Context, thing keySaver) {
	objType := thing.typeName()
	capMap, err := multitenancy.NewCapabilityMap(c.Request)
	if err != nil {
		c.JSON(http.StatusInternalServerError,
			NewError(fmt.Sprintf("list: failed to get the capmap: %v", err)))
		return
	}
	if err := backend.load(thing); err != nil {
		c.Data(http.StatusConflict, gin.MIMEJSON, nil)
		return
	}
	if !capMap.HasCapability(thing.tenantId(), objType+"_DESTROY") {
		c.Data(http.StatusConflict, gin.MIMEJSON, nil)
		return
	}
	if err := backend.remove(thing); err != nil {
		c.JSON(http.StatusConflict, NewError(fmt.Sprintf("Failed to delete %s: %v", thing.key(), err)))
		return
	}
	c.Data(http.StatusAccepted, gin.MIMEJSON, nil)
}
