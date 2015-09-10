Feature: Networks
  In order to manage networks through the network API A remote program
  wants to be able to list, show, create, edit, and delete networks

  Scenario: REST Network List
    When REST gets the {object:network} list
    Then the page returns {integer:200}

  Scenario: REST Admin Network 
    When REST gets the {object:network} "admin"
    Then the {object:network} is properly formatted
      And key "v6prefix" should not be "auto"
      And key "v6prefix" should not be "null"
      And key "v6prefix" should match "null|([a-f0-9]){1,4}:([a-f0-9]){1,4}:([a-f0-9]){1,4}:([a-f0-9]){1,4}"
  
  Scenario: REST JSON check
    When REST creates the {object:network} "jsoncheck"
    Then the {object:network} is properly formatted
    Finally REST removes the {object:network} "jsoncheck"

  Scenario: REST Admin Net Confirm
    When REST gets the {object:network} "admin"
    Then the {object:network} is properly formatted

  Scenario: Auto v6 sets range
    When I use the Network API to create "v6a" with v6prefix of "auto"
    Then the {object:network} is properly formatted
      And key "v6prefix" should not be "auto"
      And key "v6prefix" should match "([a-f0-9]){1,4}:([a-f0-9]){1,4}:([a-f0-9]){1,4}:([a-f0-9]){1,4}"
    Finally REST removes the {object:network} "v6a"

  Scenario: v6 sets range
    When I use the Network API to create "v6b" with v6prefix of "f00d:beef:cafe:dead"
    Then the {object:network} is properly formatted
      And key "v6prefix" should not be "auto"
      And key "v6prefix" should match "f00d:beef:cafe:dead"
      And key "v6prefix" should match "([a-f0-9]){1,4}:([a-f0-9]){1,4}:([a-f0-9]){1,4}:([a-f0-9]){1,4}"
    Finally REST removes the {object:network} "v6b"

  Scenario: Install API Call Works Net
    Given I use the Network API to create "foo1" with range "bar1" from "10.10.14.100/24" to "10.10.14.200/24"
    When REST gets the {object:network} "foo1" 
    Then key "name" should be "foo1"
      And key "v6prefix" should be "null"
    Finally REST removes the {object:network} "foo1"

  Scenario: Install API Call Works Range
    Given I use the Network API to create "foo2" with range "bar2" from "10.10.13.100/24" to "10.10.13.200/24"
    When REST gets the {object:network} "foo2" {object:range} "bar2" 
    Then key "name" should be "bar2"
      And the {object:range} is properly formatted
    Finally REST removes the {object:network} "foo2"

  Scenario: Network List
    Given I use the Network API to create "bdd_network" with range "bdd11" from "10.10.11.100/24" to "10.10.11.200/24"
    When I go to the "networks" page
    Then I should see {bdd:rebar.i18n.networks.index.title}
      And I should see "admin" in section "main_body"
      And I should see "bdd_network" in section "main_body"
      And there should be no translation errors
    Finally REST removes the {object:network} "bdd_network"

 Scenario: Network List to Item
    Unless travis
    Given I use the Network API to create "bdd_test12" with range "bdd12" from "10.10.12.100/24" to "10.10.12.200/24"
      And I am on the "networks" page
    When I click on the "bdd_test12" link
    Then I should see "bdd_test12"
      And there should be no translation errors
    Finally REST removes the {object:network} "bdd_test12"

  Scenario: REST Get 404
    When REST gets the {object:network} "thisdoesnotexist"
    Then I get a {integer:404} error

  Scenario: Interfaces List
    When I go to the "interfaces" page
    Then I should see {bdd:rebar.i18n.interfaces.index.title}
      And there should be no translation errors

  Scenario: Interface Add to List
    Skip may have changed, review!
    Given I add an Interface "bdd_test" with map "foo | bar"
    When I go to the "interfaces" page
    Then I should see "bdd_test"
      And I should see an input box with "foo | bar"
      And there should be no translation errors
