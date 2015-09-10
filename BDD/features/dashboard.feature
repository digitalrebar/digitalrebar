Feature: Dashboard
  In order monitor the sytem
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Basic Screen
    Skip TODO ZEHICLE disable during refactoring
    When I go to the "dashboard" page
    Then I should see "Node Dashboard"
      And I should see a link to "Add Group"
      And I should see "nodes available in the system"
      And I should see "You may regroup nodes by dragging"

  Scenario: Basic Screen
    Skip TODO ZEHICLE disable during refactoring
    Given REST creates the {object:node} "dashboard1.rebar.com"
    When I go to the "dashboard" page
    Then I should see "Node Dashboard"
      And I should see a link to "Add Group"
      And I should see "nodes available in the system"
      And I should see "dashboard1"
      And I should see "You may regroup nodes by dragging"
    Finally REST removes {object:node} "dashboard1.rebar.com"

  Scenario: Dashboard Fingerprint
    Skip TODO ZEHICLE disable during refactoring
    Given I am on the "dashboard" page
    When I examine the dashboard fingerprint
    Then the dashboard fingerprint should match the REST fingerprint