Feature: Crowbar CLI
  In order to use the system quickly
  The system operator, Oscar
  wants to use a command line interface

  Scenario: CLI has help
    Skip TODO ZEHICLE disable during refactoring
    Unless windows
    Given CLI is {apply:crowbar.g.cli}
    When I run the "users help" command
    Then the CLI should return "Usage:"
      And the CLI should return "--help"
      And the CLI should return "--username"
      And the CLI should return "--password"    
      And the CLI should return "--url"    
    
  Scenario: CLI Connects
    Skip TODO ZEHICLE disable during refactoring
    Unless windows
    Given CLI is {apply:crowbar.g.cli}
    When I run the "users list" command
    Then the CLI should return "crowbar"
      And the CLI should return "machine-install"
      And the CLI should return "developer"
      
  Scenario: Machines List
    Skip TODO ZEHICLE disable during refactoring
    Unless windows
    Given there is a {object:node} "cli.cr0wbar.com"
    Given CLI is {apply:crowbar.g.cli} 
    When I run the "machines list" command
    Then the CLI should return "cli.cr0wbar.com"
      And the CLI should return "global-node.testing.com""
    Finally REST removes {object:node} "cli.cr0wbar.com"
    
  Scenario: Curl Digest Logs Test
    Skip TODO ZEHICLE disable during refactoring
    Unless Windows
    When CURL calls "/support/logs"
    Then the CLI should return "Content-Type: text/html"
      And the CLI should not return "my/users/sign_in"

  Scenario: Curl Digest CLI download
    Skip TODO ZEHICLE disable during refactoring
    Unless Windows
    When CURL calls "/support/get_cli"
    Then the CLI should return "Content-Type: text/html"
      And the CLI should not return "my/users/sign_in"
