package main

import crowbar "github.com/VictorLowther/crowbar-api"

//	"github.com/spf13/cobra"
//	"log"

func init() {
	maker := func() crowbar.Crudder { return &crowbar.Barclamp{} }
	singularName := "barclamp"
	barclamps := makeCommandTree(singularName, maker)
	/* Not actaully implemented yet
	barclamps.AddCommand(&cobra.Command{
		Use: "import [path]",
		Short: "Import a barclamp from [path]",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires exactly 1 argument\n",c.UseLine())
			}
			res, err := crowbar.BarclampImport(args[0])
			if err != nil {
				log.Fatalf("Error importing barclamp from %v!\n Error: %v\n",args[0], err.Error())
			}
			fmt.Println(prettyJSON(res))
		},
	}) */
	app.AddCommand(barclamps)
}
