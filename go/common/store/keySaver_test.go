package store

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"testing"
)

var (
	currentStore SimpleStore
	failed       = errors.New("Failed hook")
)

type TestVal struct {
	Name     string
	Val      string
	hook     string
	failHook bool
}

func (t *TestVal) doHook(hook string) error {
	t.hook = hook
	if t.failHook {
		return failed
	}
	return nil
}

func (t *TestVal) Prefix() string {
	return "testVal"
}

func (t *TestVal) Key() string {
	return t.Name
}

func (t *TestVal) New() KeySaver {
	return &TestVal{}
}

func (t *TestVal) Backend() SimpleStore {
	return currentStore
}

func (t *TestVal) OnLoad() error {
	return t.doHook("OnLoad")
}

func (t *TestVal) OnChange(KeySaver) error {
	return t.doHook("OnChange")
}

func (t *TestVal) BeforeDelete() error {
	return t.doHook("BeforeDelete")
}

func (t *TestVal) AfterDelete() {
	t.doHook("AfterDelete")
}

func (t *TestVal) OnCreate() error {
	return t.doHook("OnCreate")
}

func (t *TestVal) BeforeSave() error {
	return t.doHook("BeforeSave")
}

func (t *TestVal) AfterSave() {
	t.doHook("AfterSave")
}

type op func(KeySaver) (bool, error)
type test struct {
	op       op
	name     string
	val      *TestVal
	pass     bool
	hookFail bool
	lastHook string
}

var tv = TestVal{"Name", "Value", "", false}
var ntv = TestVal{"Uncreated", "Value", "", false}
var tests = []test{
	test{op(Create), "Create Hook Fail", &tv, false, true, "OnCreate"},
	test{op(Create), "Create Succeed", &tv, true, false, "AfterSave"},
	test{op(Create), "Create Duplicate Fail", &tv, false, false, ""},
	test{op(Load), "Load Hook Fail", &tv, true, true, "OnLoad"},
	test{op(Load), "Load Nonexistent Fail", &ntv, false, false, ""},
	test{op(Load), "Load Succeed", &tv, true, false, "OnLoad"},
	test{op(Save), "Save Before Hook Fail", &tv, false, true, "BeforeSave"},
	test{op(Save), "Save Succeed", &tv, true, false, "AfterSave"},
	test{op(Update), "Update Before Hook Fail", &tv, false, true, "OnChange"},
	test{op(Update), "Update Succeed", &tv, true, false, "AfterSave"},
	test{op(Remove), "Remove Hook Fail", &tv, false, true, "BeforeDelete"},
	test{op(Remove), "Remove Success", &tv, true, false, "AfterDelete"},
	test{op(Remove), "Remove Nonexistent Fail", &ntv, false, false, "BeforeDelete"},
}

// Expects a freshly-created store
func testStore(t *testing.T) {
	for _, s := range tests {
		expectedTo := "fail"
		if s.pass {
			expectedTo = "pass"
		}
		actuallyDid := "fail"
		s.val.hook = ""
		s.val.failHook = s.hookFail
		ok, err := s.op(s.val)
		if ok {
			actuallyDid = "pass"
		}
		passMsg := fmt.Sprintf("%s: Expected to %s, actually %s", s.name, expectedTo, actuallyDid)
		hookMsg := fmt.Sprintf("%s: Expected last hook to be `%s`, was `%s`", s.name, s.lastHook, s.val.hook)
		if ok != s.pass {
			t.Error(passMsg)
		} else {
			t.Log(passMsg)
		}
		if s.lastHook != s.val.hook {
			t.Error(hookMsg)
		} else {
			t.Log(hookMsg)
		}
		if s.hookFail {
			if err == nil {
				t.Errorf("%s: Expected hook to fail, but got no error!", s.name)
			} else if err != failed {
				t.Errorf("%s: Expected hook to fail with `%v`, but got `%v`", s.name, failed, err)
			} else {
				t.Logf("%s: Got expected hook failure `%v`", s.name, failed)
			}
		} else if !s.pass {
			if err != nil {
				t.Logf("%s: Got error %v", s.name, err)
			} else {
				t.Logf("%s: Expected to fail, but got no error!", s.name)
			}
		}
	}
	// At the end, the store should be empty
	ents, err := ListRaw(currentStore)
	if err != nil {
		t.Errorf("Error listing store: %v", err)
	} else if len(ents) != 0 {
		t.Errorf("Too many entries in store: wanted 0, got %d", len(ents))
	}
}

func TestMemoryStore(t *testing.T) {
	currentStore = NewSimpleMemoryStore()
	t.Log("Testing simple memory store")
	testStore(t)
	t.Log("Memory store test finished")
}

func TestLocalStore(t *testing.T) {
	tmpDir, err := ioutil.TempDir("", "localstore-")
	if err != nil {
		t.Errorf("Failed to create tmp dir for LocalStore testing")
		return
	}
	defer os.RemoveAll(tmpDir)
	currentStore, err = NewSimpleLocalStore(tmpDir)
	if err != nil {
		t.Errorf("Failed to create local store: %v", err)
		return
	}
	t.Log("Testing local store")
	testStore(t)
	t.Log("Local store test finished")
}

func TestFileStore(t *testing.T) {
	tmpDir, err := ioutil.TempDir("", "filestore-")
	if err != nil {
		t.Errorf("Failed to create tmp dir for FileStore testing")
		return
	}
	defer os.RemoveAll(tmpDir)
	currentStore, err = NewFileBackend(tmpDir)
	if err != nil {
		t.Errorf("Failed to create file store: %v", err)
		return
	}
	t.Log("Testing file store")
	testStore(t)
	t.Log("File store test finished")
}
