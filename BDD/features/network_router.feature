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
      And key "address" should be "10.10.99.1/32"
      And key "pref" should be "42"
  
  Scenario: REST JSON create
    Given I use the Network API to create "router1" with range "bar1" from "10.10.15.100/24" to "10.10.15.200/24"
    When REST creates the {object:network_router} "10.10.15.1/24" on network "router1"
    Then key "address" should be "10.10.15.1/24"
      And key "pref" should be "65536"
    Finally REST deletes the {object:network} "router1"

  Scenario: REST JSON update
    Given I use the Network API to create "router2" with range "bar2" from "10.10.16.100/24" to "10.10.16.200/24"
      And REST creates the {object:network_router} "10.10.16.1/24" on network "router2"
    When REST sets {object:network_router} on "router2" item "pref" to "100"
    Then key "address" should be "10.10.16.1/24"
      And key "pref" should be "100"
      And key "pref" should not be "65536"
    Finally REST deletes the {object:network} "router2"

  Scenario: REST Cannot change Network
    Given I use the Network API to create "router3" with range "bar2" from "10.10.17.100/24" to "10.10.17.200/24"
      And REST creates the {object:network_router} "10.10.17.1" on network "router3"
    When REST sets {object:network_router} on "router3" item "network_id" to "1"
    Then key "network_id" should not be "1"
    Finally REST deletes the {object:network} "router3"

  Scenario: REST JSON delete
    Given I use the Network API to create "router4" with range "bar2" from "10.10.18.100/24" to "10.10.18.200/24"
      And REST creates the {object:network_router} "10.10.18.1" on network "router4"
    When REST deletes {object:network_router} on "router4"
    Then there is no {object:network_router} on network "router4"
    Finally REST deletes the {object:network} "router4"

  Scenario: Network UI shows Route
    When I go to the "networks/testrouter" page
    Then I should see "42"
      And I should see "10.10.99.200"

  Scenario: UI for NetworkRouters
    When I go to the "network_routers" page
    Then I should see {bdd:rebar.i18n.network_routers.index.title}
      And there should be no translation errors

  Scenario: UI for Network-NetworkRouters
    When I go to the "networks/testrouter/network_routers" page
    Then I should see {bdd:rebar.i18n.network_routers.index.title}
      And there should be no translation errors

  Scenario: UI for Network-NetworkRouters
    When I go to the "networks/testrouter/network_routers/anys" page
    Then I should see {bdd:rebar.i18n.network_routers.show.title}
      And there should be no translation errors
