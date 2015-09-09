Feature: Dashboard GetReady
  In order setup my system
  The system operator, Oscar
  wants to be able to quickly allocate deployment, network and node O/S

  Scenario: UI Node List
    When I go to the "dashboard/getready" page
    Then I should see {bdd:crowbar.i18n.dashboard.getready.title}
      And I should see an input box "deployment" with {bdd:crowbar.i18n.dashboard.getready.default}
      And I should see an input box "range" with {bdd:crowbar.i18n.dashboard.getready.range_base}
      And I should see an input box "conduit" with "1g0"
      And I should see an input box "first_ip" with "10.10.10.10/24"
      And I should see an input box "last_ip" with "10.10.10.250/24"
      And I should see {lookup:dashboard_getready.name}
      And I should not see {lookup:crowbar.node_name}
      And there should be no translation errors

  Scenario: UI Node List Click to Node
    Given I am on the "dashboard/getready" page
    When I click on the {lookup:dashboard_getready.name} link
    Then I should see {lookup:dashboard_getready.name}
      And there should be no translation errors

  Scenario: GetReady Create Deployment
    Given there is not a {object:deployment} "getready"
      And I post {fields:deployment=getready} to "dashboard/getready"
    When REST gets the {object:deployment} "getready" 
    Then I get a {integer:200} result
      And key "name" should be "getready"
      And the {object:deployment} is properly formatted
    Finally REST removes the {object:deployment} "getready"

  Scenario: GetReady Create Network
    Given there is not a {object:deployment} "getready"
      And I post {fields:deployment=getready&conduit=2g2} to "dashboard/getready"
    When REST gets the {object:network} "getready" 
    Then I get a {integer:200} result
      And key "name" should be "getready"
      And key "conduit" should be "2g2"
      And the {object:network} is properly formatted
    Finally REST removes the {object:network} "getready"
      And REST removes the {object:deployment} "getready"
