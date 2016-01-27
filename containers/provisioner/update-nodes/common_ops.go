package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/VictorLowther/jsonpatch"
	"github.com/labstack/echo"
)

func listThings(c *echo.Context, thing keySaver) error {
	things := backend.List(thing)
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

func createThing(c *echo.Context, thing keySaver) error {
	if err := c.Bind(&thing); err != nil {
		return c.JSON(http.StatusBadRequest, NewError(err.Error()))
	}
	if err := backend.Load(thing); err == nil {
		return c.JSON(http.StatusConflict, NewError(thing.Key()+" already exists."))
	}
	if err := backend.Save(thing, nil); err != nil {
		return c.JSON(http.StatusInternalServerError, NewError(err.Error()))
	}
	return c.JSON(http.StatusCreated, thing)
}

func getThing(c *echo.Context, thing keySaver) error {
	if err := backend.Load(thing); err != nil {
		return c.NoContent(http.StatusNotFound)
	}
	return c.JSON(http.StatusOK, thing)
}

func updateThing(c *echo.Context, oldThing, newThing keySaver) error {
	if err := backend.Load(oldThing); err != nil {
		return c.NoContent(http.StatusNotFound)
	}
	patch, err := ioutil.ReadAll(c.Request().Body)
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
	if err := backend.Save(newThing, oldThing); err != nil {
		return err
	}
	return c.JSON(http.StatusAccepted, newThing)
}

func deleteThing(c *echo.Context, thing keySaver) error {
	if backend.Delete(thing) != nil {
		return c.NoContent(http.StatusConflict)
	}
	return c.NoContent(http.StatusAccepted)
}
