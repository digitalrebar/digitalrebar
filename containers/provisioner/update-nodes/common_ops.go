package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/VictorLowther/jsonpatch"
	"github.com/labstack/echo"
)

func listThings(c echo.Context, thing keySaver) error {
	things := backend.list(thing)
	res := make([]interface{}, len(things))
	for i, obj := range things {
		var buf interface{}
		if err := json.Unmarshal(obj, &buf); err != nil {
			return c.JSON(http.StatusInternalServerError,
				NewError(fmt.Sprintf("list: error unmarshalling %v: %v", string(obj), err)))
		}
		res[i] = buf
	}
	return c.JSON(http.StatusOK, res)
}

func createThing(c echo.Context, newThing keySaver) error {
	if err := c.Bind(&newThing); err != nil {
		return c.JSON(http.StatusBadRequest, NewError(err.Error()))
	}
	finalStatus := http.StatusCreated
	oldThing := newThing.newIsh()
	if err := backend.load(oldThing); err == nil {
		logger.Infof("backend: Updating %v\n", oldThing.key())
		finalStatus = http.StatusAccepted
	} else {
		logger.Infof("backend: Creating %v\n", newThing.key())
		oldThing = nil
	}
	if err := backend.save(newThing, oldThing); err != nil {
		return c.JSON(http.StatusConflict, NewError(err.Error()))
	}
	return c.JSON(finalStatus, newThing)
}

func getThing(c echo.Context, thing keySaver) error {
	if err := backend.load(thing); err != nil {
		return c.NoContent(http.StatusNotFound)
	}
	return c.JSON(http.StatusOK, thing)
}

func updateThing(c echo.Context, oldThing, newThing keySaver) error {
	if err := backend.load(oldThing); err != nil {
		return c.NoContent(http.StatusNotFound)
	}
	patch, err := ioutil.ReadAll(c.Request().Body())
	if err != nil {
		return err
	}
	oldThingBuf, _ := json.Marshal(oldThing)
	newThingBuf, err, loc := jsonpatch.ApplyJSON(oldThingBuf, patch)
	if err != nil {
		return c.JSON(http.StatusConflict, NewError(fmt.Sprintf("Failed to apply patch at %d: %v\n", loc, err)))
	}
	if err := json.Unmarshal(newThingBuf, &newThing); err != nil {
		return err
	}
	if err := backend.save(newThing, oldThing); err != nil {
		return c.JSON(http.StatusConflict, NewError(err.Error()))
	}
	return c.JSON(http.StatusAccepted, newThing)
}

func deleteThing(c echo.Context, thing keySaver) error {
	if err := backend.remove(thing); err != nil {
		return c.JSON(http.StatusConflict, NewError(fmt.Sprintf("Failed to delete %s: %v", thing.key(), err)))
	}
	return c.NoContent(http.StatusAccepted)
}
