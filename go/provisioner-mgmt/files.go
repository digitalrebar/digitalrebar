package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path"
	"strings"

	"github.com/gin-gonic/gin"
)

func listFiles(c *gin.Context, fileRoot string) {
	user := getUser(c)
	res := []string{}
	if !testCap(c, int(user.TenantID), "PROVISIONER_FILE_READ") {
		c.JSON(http.StatusOK, res)
		return
	}
	pathPart, _ := c.GetQuery("path")
	ents, err := ioutil.ReadDir(path.Join(fileRoot, "files", path.Clean(pathPart)))
	if err != nil {
		c.JSON(http.StatusNotFound, NewError(fmt.Sprintf("list: error listing files: %v", err)))
		return
	}
	for _, ent := range ents {
		if ent.Mode().IsRegular() {
			res = append(res, ent.Name())
		} else if ent.Mode().IsDir() {
			res = append(res, ent.Name()+"/")
		}
	}
	c.JSON(http.StatusOK, res)
}

func getFile(c *gin.Context, fileRoot, name string) {
	user := getUser(c)
	if !testCap(c, int(user.TenantID), "PROVISIONER_FILE_READ") {
		c.Data(http.StatusNotFound, "application/octet-stream", nil)
		return
	}
	fileName := path.Join(fileRoot, `files`, path.Clean(name))
	c.File(fileName)
}

func uploadFile(c *gin.Context, fileRoot, name string) {
	user := getUser(c)
	if !testCap(c, int(user.TenantID), "PROVISIONER_FILE_CREATE") {
		c.JSON(http.StatusForbidden, NewError("upload: access denied"))
		return
	}
	if c.Request.Header.Get(`Content-Type`) != `application/octet-stream` {
		c.JSON(http.StatusUnsupportedMediaType, NewError(fmt.Sprintf("upload: file %s must have content-type application/octet-stream", name)))
		return
	}
	fileTmpName := path.Join(fileRoot, `files`, fmt.Sprintf(`.%s.part`, path.Clean(name)))
	fileName := path.Join(fileRoot, `files`, path.Clean(name))
	if strings.HasSuffix(fileName, "/") {
		c.JSON(http.StatusForbidden, NewError(fmt.Sprintf("upload: Cannot upload a directory")))
		return
	}
	if err := os.MkdirAll(path.Dir(fileName), 0755); err != nil {
		c.JSON(http.StatusConflict, NewError(fmt.Sprintf("upload: unable to create directory %s", path.Clean(path.Dir(name)))))
		return
	}
	if _, err := os.Open(fileTmpName); err == nil {
		c.JSON(http.StatusConflict, NewError(fmt.Sprintf("upload: file %s already uploading", name)))
		return
	}
	tgt, err := os.Create(fileTmpName)
	if err != nil {
		c.JSON(http.StatusConflict, NewError(fmt.Sprintf("upload: Unable to upload %s: %v", name, err)))
		return
	}

	copied, err := io.Copy(tgt, c.Request.Body)
	if err != nil {
		os.Remove(fileTmpName)
		c.JSON(http.StatusInsufficientStorage, NewError(fmt.Sprintf("upload: Failed to upload %s: %v", name, err)))
		return
	}
	if c.Request.ContentLength != 0 && copied != c.Request.ContentLength {
		os.Remove(fileTmpName)
		c.JSON(http.StatusBadRequest, NewError(fmt.Sprintf("upload: Failed to upload entire file %s: %d bytes expected, %d bytes recieved", name, c.Request.ContentLength, copied)))
		return
	}
	os.Remove(fileName)
	os.Rename(fileTmpName, fileName)
	res := &struct {
		Name string
		Size int64
	}{name, copied}
	c.JSON(http.StatusCreated, res)
}

func deleteFile(c *gin.Context, fileRoot, name string) {
	user := getUser(c)
	if !testCap(c, int(user.TenantID), "PROVISIONER_FILE_DESTROY") {
		c.JSON(http.StatusForbidden, NewError("delete: access denied"))
		return
	}
	fileName := path.Join(fileRoot, `files`, path.Clean(name))
	if fileName == path.Join(fileRoot, `files`) {
		c.JSON(http.StatusForbidden, NewError("delete: Not allowed to remove files dir"))
		return
	}
	if err := os.Remove(fileName); err != nil {
		c.JSON(http.StatusNotFound, NewError(fmt.Sprintf("delete: unable to delete %s: %v", name, err)))
		return
	}
	c.JSON(http.StatusAccepted, nil)
}
