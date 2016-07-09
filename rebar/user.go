package main

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/digitalrebar/rebar-api/api"
	"github.com/spf13/cobra"
)

func init() {
	maker := func() api.Crudder { return &api.User{} }
	singularName := "user"
	tree := makeCommandTree(singularName, maker)
	tree.AddCommand(
		&cobra.Command{
			Use:   "capabilities [id]",
			Short: "Get the capabilities of this user",
			Run: func(c *cobra.Command, args []string) {
				if len(args) != 1 {
					log.Fatalf("%v requires 1 arguments", c.UseLine())
				}
				obj := &api.User{}
				if err := session.Fetch(obj, args[0]); err != nil {
					log.Fatalln("Unable to fetch user from the server", err)
				}
				capmap, err := obj.Capabilities()
				if err != nil {
					log.Fatalln("Unable to get capabilities", err)
				}
				fmt.Println(prettyJSON(capmap))
			},
		},
		&cobra.Command{
			Use:   "password [id] to [password]",
			Short: "Set a users password to [password]",
			Run: func(c *cobra.Command, args []string) {
				if len(args) != 3 {
					log.Fatalf("%v requires 2 arguments", c.UseLine())
				}
				obj := &api.User{}
				if err := session.Fetch(obj, args[0]); err != nil {
					log.Fatalln("Unable to fetch user from the server", err)
				}
				token, err := obj.StartPasswordReset()
				if err != nil {
					log.Fatalln("Unable to get password reset token", err)
				}
				if err := obj.CompletePasswordReset(token, args[2]); err != nil {
					log.Fatalln("Unable to change user password", err)
				}
				fmt.Println(prettyJSON(obj))
			},
		},
		&cobra.Command{
			Use:   "import [json]",
			Short: "Import a user.  Will create a new user and set the password in one shot",
			Run: func(c *cobra.Command, args []string) {
				if len(args) != 1 {
					log.Fatalf("%v requires 1 argument", c.UseLine())
				}
				obj := &api.User{}
				if err := session.CreateJSON(obj, []byte(args[0])); err != nil {
					if _, err := obj.Id(); err != nil {
						log.Fatalln("User has no name", err)
					}
					if err := session.Read(obj); err != nil {
						log.Fatalln("Unable to create or fetch user", err)
					}
					if err := session.UpdateJSON(obj, []byte(args[0])); err != nil {
						log.Fatalln("Unable to update user", err)
					}
				}
				rest := make(map[string]interface{})
				if err := json.Unmarshal([]byte(args[0]), &rest); err != nil {
					log.Fatalln("Problem unmarshalling new user", err)
				}
				password, ok := rest["password"]
				if ok {
					realPassword, ok := password.(string)
					if !ok {
						log.Fatal("Password not a string!")
					}
					token, err := obj.StartPasswordReset()
					if err != nil {
						log.Fatalln("Unable to get password reset token", err)
					}
					if err := obj.CompletePasswordReset(token, realPassword); err != nil {
						log.Fatalln("Unable to change user password", err)
					}
				}
				fmt.Println(prettyJSON(obj))
			},
		},
	)
	app.AddCommand(tree)
}
