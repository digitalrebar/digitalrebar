package main

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path"

	"github.com/gin-gonic/gin"
)

func listIsos(c *gin.Context, fileRoot string) {
	user := getUser(c)
	res := []string{}
	if !testCap(c, int(user.TenantID), "PROVISIONER_FILE_READ") {
		c.JSON(http.StatusOK, res)
		return
	}
	ents, err := ioutil.ReadDir(path.Join(fileRoot, "isos"))
	if err != nil {
		c.JSON(http.StatusNotFound, NewError(fmt.Sprintf("list: error listing isos: %v", err)))
		return
	}
	for _, ent := range ents {
		if !ent.Mode().IsRegular() {
			continue
		}
		res = append(res, ent.Name())
	}
	c.JSON(http.StatusOK, res)
}

func getIso(c *gin.Context, fileRoot, name string) {
	user := getUser(c)
	if !testCap(c, int(user.TenantID), "PROVISIONER_FILE_READ") {
		c.Data(http.StatusNotFound, "application/octet-stream", nil)
		return
	}
	isoName := path.Join(fileRoot, `isos`, path.Base(name))
	c.File(isoName)
}

func reloadBootenvsForIso(name string) {
	env := &BootEnv{}
	newEnv := &BootEnv{}
	for _, blob := range backend.list(env) {
		if err := json.Unmarshal(blob, env); err != nil {
			continue
		}
		if env.Available || env.OS.IsoFile != name {
			continue
		}
		json.Unmarshal(blob, newEnv)
		newEnv.Available = true
		backend.save(newEnv, env)
	}
}

func uploadIso(c *gin.Context, fileRoot, name string) {
	user := getUser(c)
	if !testCap(c, int(user.TenantID), "PROVISIONER_FILE_CREATE") {
		c.JSON(http.StatusForbidden, NewError("upload: access denied"))
		return
	}
	if c.Request.Header.Get(`Content-Type`) != `application/octet-stream` {
		c.JSON(http.StatusUnsupportedMediaType, NewError(fmt.Sprintf("upload: iso %s must have content-type application/octet-stream", name)))
		return
	}
	isoTmpName := path.Join(fileRoot, `isos`, fmt.Sprintf(`.%s.part`, path.Base(name)))
	isoName := path.Join(fileRoot, `isos`, path.Base(name))
	if _, err := os.Open(isoTmpName); err == nil {
		c.JSON(http.StatusConflict, NewError(fmt.Sprintf("upload: iso %s already uploading", name)))
		return
	}
	tgt, err := os.Create(isoTmpName)
	if err != nil {
		c.JSON(http.StatusConflict, NewError(fmt.Sprintf("upload: Unable to upload %s: %v", name, err)))
	}

	copied, err := io.Copy(tgt, c.Request.Body)
	if err != nil {
		os.Remove(isoTmpName)
		c.JSON(http.StatusInsufficientStorage, NewError(fmt.Sprintf("upload: Failed to upload %s: %v", name, err)))
		return
	}
	if c.Request.ContentLength != 0 && copied != c.Request.ContentLength {
		os.Remove(isoTmpName)
		c.JSON(http.StatusBadRequest, NewError(fmt.Sprintf("upload: Failed to upload entire file %s: %d bytes expected, %d bytes recieved", name, c.Request.ContentLength, copied)))
		return
	}
	os.Remove(isoName)
	os.Rename(isoTmpName, isoName)
	res := &struct {
		Name string
		Size int64
	}{name, copied}
	go reloadBootenvsForIso(name)
	c.JSON(http.StatusCreated, res)
}

func deleteIso(c *gin.Context, fileRoot, name string) {
	user := getUser(c)
	if !testCap(c, int(user.TenantID), "PROVISIONER_FILE_DESTROY") {
		c.JSON(http.StatusForbidden, NewError("delete: access denied"))
		return
	}
	isoName := path.Join(fileRoot, `isos`, path.Base(name))
	if err := os.Remove(isoName); err != nil {
		c.JSON(http.StatusNotFound, NewError(fmt.Sprintf("delete: unable to delete %s: %v", name, err)))
		return
	}
	c.JSON(http.StatusAccepted, nil)
}
