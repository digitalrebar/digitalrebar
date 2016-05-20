package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/VictorLowther/jsonpatch"
	"github.com/gin-gonic/gin"
)

func listThings(c *gin.Context, thing keySaver) {
	things := backend.list(thing)
	res := make([]interface{}, len(things))
	for i, obj := range things {
		var buf interface{}
		if err := json.Unmarshal(obj, &buf); err != nil {
			c.JSON(http.StatusInternalServerError,
				NewError(fmt.Sprintf("list: error unmarshalling %v: %v", string(obj), err)))
                        return
		}
		res[i] = buf
	}
	c.JSON(http.StatusOK, res)
}

func createThing(c *gin.Context, newThing keySaver) {
	if err := c.Bind(&newThing); err != nil {
		c.JSON(http.StatusBadRequest, NewError(err.Error()))
                return
	}
	finalStatus := http.StatusCreated
	oldThing := newThing.newIsh()
	if err := backend.load(oldThing); err == nil {
		logger.Printf("backend: Updating %v\n", oldThing.key())
		finalStatus = http.StatusAccepted
	} else {
		logger.Printf("backend: Creating %v\n", newThing.key())
		oldThing = nil
	}
	if err := backend.save(newThing, oldThing); err != nil {
		c.JSON(http.StatusConflict, NewError(err.Error()))
                return
	}
	c.JSON(finalStatus, newThing)
}

func getThing(c *gin.Context, thing keySaver) {
	if err := backend.load(thing); err != nil {
		c.Data(http.StatusNotFound, gin.MIMEJSON, nil)
                return
	}
	c.JSON(http.StatusOK, thing)
}

func updateThing(c *gin.Context, oldThing, newThing keySaver) {
	if err := backend.load(oldThing); err != nil {
		c.Data(http.StatusNotFound, gin.MIMEJSON, nil)
                return
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
	if err := backend.save(newThing, oldThing); err != nil {
		c.JSON(http.StatusConflict, NewError(err.Error()))
                return
	}
	c.JSON(http.StatusAccepted, newThing)
}

func deleteThing(c *gin.Context, thing keySaver) {
	if err := backend.remove(thing); err != nil {
		c.JSON(http.StatusConflict, NewError(fmt.Sprintf("Failed to delete %s: %v", thing.key(), err)))
                return
	}
	c.Data(http.StatusAccepted, gin.MIMEJSON, nil)
}
