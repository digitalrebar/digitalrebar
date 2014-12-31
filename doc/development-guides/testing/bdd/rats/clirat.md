##### CLI Steps (clirat)

The CLI Steps file is designed to test CLIs for the BDD framework.

The core function of the CLI steps is provided by the Erlang `os:cmd` instruction.  You may need to combine the path to the desired executable with the CLI.  For example, "cd ../bin && ./crowbar" in order to run the CLI.

There are multiple ways to identify which CLI to use:

* In config, you may add key for the CLI.  `{cli, "cd ../bin && ./crowbar"}.`
* In another erl file, you could add a `g(cli)... cli -> "[cmd]";` instruction and call it from the given step using `Given CLI is {apply:crowbar.g.cli}`
* In the given step, you can define the CLI in place `Given CLI is "cd ../bin && ./crowbar"`

Once you have defined the CLI location, you can run commands using the `When I run the "command here" command`.  This command will capture the output into a list (`{cli, ["reponse line 1", "response line 2", "response line N"]}`).

You can then check the responses using the `Then the CLI should return "expected result"` steps.

> OS Specific?  You can step tests that only work on Linux using the `Unless windows` or `While linux` step definition.

The CLI command will automatically add the following parameters to the CLI call:

* --user [user]
* --password [password]
* --url [url]

The values for the parameters are resolved from the configuration file.

For example:

    Scenario: Machines List
      Unless windows
      Given there is a {object:node} "cli.cr0wbar.com"
      Given CLI is {apply:crowbar.g.cli}
      When I run the "machines list" command
      Then the CLI should return "cli.cr0wbar.com"
        And the CLI should return "global-node.testing.com""
      Finally REST removes {object:node} "cli.cr0wbar.com"
