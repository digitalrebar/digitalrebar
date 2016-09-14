package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"

	"github.com/VictorLowther/yaml"
	"github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	"github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"
	"github.com/spf13/cobra"
)

func init() {
	maker := func() api.Crudder { return &api.Barclamp{} }
	singularName := "barclamp"
	barclamps := makeCommandTree(singularName, maker)
	barclamps.AddCommand(&cobra.Command{
		Use:   "import [path]",
		Short: "Import a barclamp from [path]",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires exactly 1 argument\n", c.UseLine())
			}
			toLoad := args[0]
			for {
				info, err := os.Stat(toLoad)
				if err != nil {
					log.Fatalf("Error probing %v\n%v\n", toLoad, err)
				}
				if info.IsDir() {
					toLoad = path.Join(toLoad, "rebar.yml")
					continue
				}
				if !info.Mode().IsRegular() {
					log.Fatalf("%v is not a regular file!", toLoad)
				}
				break
			}

			dir, f := path.Split(toLoad)
			if f != "rebar.yml" {
				log.Fatalf("%v is not the location of a rebar.yml file!", args[0])
			}

			loadPaths := []string{toLoad}

			subBarclamps, err := filepath.Glob(path.Join(dir, "barclamps", "*.yml"))
			if err != nil {
				log.Fatal(err)
			}
			loadPaths = append(loadPaths, subBarclamps...)
			res := make([]*api.Barclamp, len(loadPaths))

			for i, toLoad := range loadPaths {
				f, err := os.Open(toLoad)
				if err != nil {
					log.Fatalf("Failed to open %v: %v\n", toLoad, err)
				}
				buf, err := ioutil.ReadAll(f)
				if err != nil {
					log.Fatalf("Failed to read all of %v\n%v\n", toLoad, err)
				}
				type bcImporter struct {
					Value *datatypes.BarclampImport `json:"value"`
				}

				bcImport := &bcImporter{}
				if err := yaml.Unmarshal(buf, &bcImport.Value); err != nil {
					log.Fatalf("Failed to unmarshal %v\n%v\n", toLoad, err)
				}
				if err := bcImport.Value.FixupYAMLImport(); err != nil {
					log.Fatalf("Failed to fix up YAML import: %v\n", err)
				}
				bcImport.Value.Barclamp.SourcePath = toLoad
				jsonBuf, err := json.Marshal(bcImport)
				if err != nil {
					log.Fatalf("Error marshalling %v to JSON: %v", toLoad, err)
				}
				bc := &api.Barclamp{}
				if err := session.Import(bc, jsonBuf); err != nil {
					log.Fatalf("Error importing %v: %v", toLoad, err)
				}
				res[i] = bc
			}

			fmt.Println(prettyJSON(res))
		},
	})
	app.AddCommand(barclamps)
}
