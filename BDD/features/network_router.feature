Feature: Network Routers
  In order to manage networks, Operator
  wants to be able to add routers

  Scenario: REST Routers List
    When REST gets the {object:network_router} list
    Then the page returns {integer:200}

  Scenario: REST Routers List Under Networks
    When REST requests the "api/v2/networks/testrouter/network_routers/" page
    Then the page returns {integer:200}

  Scenario: REST JSON check
    When REST requests the "api/v2/networks/testrouter/network_routers/any" page
    Then the {object:network_router} is properly formatted
      And key "address:address" should be "168452865"
      And key "address:subnet" should be "32"
      And key "pref" should be "42"
  
  Scenario: REST JSON create
    Given I use the Network API to create "router1" with range "bar1" from "10.10.15.100/24" to "10.10.15.200/24"
    When REST creates the {object:network_router} "10.10.14.1" on network "router1"
    Then key "address:address" should be "168431105"
      And key "address:subnet" should be "32"
      And key "pref" should be "65536"
    Finally REST deletes the {object:network} "router1"

  Scenario: REST JSON update
    Skip Zehicle

  Scenario: REST Cannot change Network
    Skip Zehicle

  Scenario: REST JSON delete
    Skip Zehicle

  Scenario: Network UI creates Route
    Skip Zehicle

  Scenario: Network UI shows Route
    When I go to the "networks/testrouter" page
    Then I should see "42"

  Scenario: UI for NetworkRouters
    When I go to the "network_routers" page
    Then I should see {bdd:crowbar.i18n.network_routers.index.title}
      And there should be no translation errors

  Scenario: UI for Network-NetworkRouters
    When I go to the "networks/testrouter/network_routers" page
    Then I should see {bdd:crowbar.i18n.network_routers.index.title}
      And there should be no translation errors

  Scenario: UI for Network-NetworkRouters
    When I go to the "networks/testrouter/network_routers/anys" page
    Then I should see {bdd:crowbar.i18n.network_routers.show.title}
      And there should be no translation errors
