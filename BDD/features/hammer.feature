Feature: Hammers API
  In order to provision applications
  The system operator, Oscar
  wants to be able to use difference interfaces

  Scenario: Hammer List
    When REST gets the {object:hammer} list
    Then the list should have an object with key "name" value "ssh"
      And the list should have an object with key "username" value "root"

  Scenario: REST JSON check
    When REST gets the {object:hammer} "ssh"
    Then the {object:hammer} is properly formatted

  Scenario: Hammers UI Page
    When I go to the "hammers" page
    Then I should see a heading {bdd:crowbar.i18n.hammers.index.title}
      And I should see "root"
      And I should see "ssh"
      And there should be no translation errors

  Scenario: Hammers UI Drill Down Test
    Given I am on the "hammers" page
    When I click on the "ssh" link
    Then I should see a heading "ssh on (.*)"
      And I should see {bdd:crowbar.i18n.hammers.show.hammer}
      And there should be no translation errors

  Scenario: Available Hammers UI Page
    When I go to the "available_hammers" page
    Then I should see a heading {bdd:crowbar.i18n.available_hammers.index.title}
      And I should see "ssh"
      And there should be no translation errors

  Scenario: Available Hammers UI Drill Down Test
    Given I am on the "available_hammers" page
    When I click on the "ssh" link
    Then I should see a heading "ssh"
      And I should see "SecureShellHammer"
      And I should see heading {bdd:crowbar.i18n.available_hammers.show.hammers}
      And there should be no translation errors

  Scenario: I get a list of hammers for a node
    Given REST creates the {object:node} "hammer.works.node"
    When REST requests the "/api/v2/nodes/hammer.works.node/hammers" page
    Then the page returns "200"
    Finally REST removes the {object:node} "hammer.works.node"

  Scenario: I get a list of power options for a node
    Given REST creates the {object:node} "power.works.node"
    When REST requests the "/api/v2/nodes/power.works.node/power" page
    Then the page returns "200"
      And Array contains key "reboot"
    Finally REST removes the {object:node} "power.works.power

  Scenario: Hammers works from CLI
    Given REST creates the {object:node} "hammer.works.cli"
      And there are no pending Crowbar runs for {o:node} "hammer.works.cli"
      And CLI is {apply:crowbar.g.cli}
    When I run the "nodes power hammer.works.cli reboot" command
    Then {object:node} "hammer.works.cli" is not alive
      And the CLI should return "reboot"
      And the CLI should return "development faked"
    Finally REST removes the {object:node} "hammer.works.cli"

  Scenario: Hammers fails from CLI
    Given REST creates the {object:node} "hammer.fails.cli"
      And there are no pending Crowbar runs for {o:node} "hammer.fails.cli"
      And CLI is {apply:crowbar.g.cli}
    When I run the "nodes power hammer.fails.cli rtfm" command
    Then {object:node} "hammer.fails.cli" is alive
      And the CLI should return "API call ok"
      And the CLI should return ":reboot"
      And the CLI should return "501"
    Finally REST removes the {object:node} "hammer.fails.cli"

  Scenario: Hammers gives 501 for invalid option
    Given REST creates the {object:node} "hammer.r501.com"
      And there are no pending Crowbar runs for {o:node} "hammer.r501.com"
    When REST tells {object:node} "hammer.r501.com" to "get_funky"    
    Then I get a "501" error
    Finally REST removes the {object:node} "hammer.r501.com"

  Scenario: Hammers works for reboot
    Given REST creates the {object:node} "hammer.works.rbt"
      And there are no pending Crowbar runs for {o:node} "hammer.works.rbt"
    When REST tells {object:node} "hammer.works.rbt" to "reboot"
    Then Array key "action" matches "reboot"
      And I get a "200" result
    Finally REST removes the {object:node} "hammer.works.rbt"
