Feature: Jigs API
  In order to provision applications
  The system operator, Oscar
  wants to be able to use a configuration management database (like Chef or Puppet)

  Scenario: Jig List
    When REST gets the {object:jig} list
    Then the list should have an object with key "name" value "script"
      And the list should have an object with key "name" value "noop"

  Scenario: REST JSON check
    When REST gets the {object:jig} "script"
    Then the {object:jig} is properly formatted

  Scenario: Jigs UI Page
    When I go to the "jigs" page
    Then I should see a heading {bdd:rebar.i18n.jigs.index.title}
      And I should see "noop"
      And I should see "script"
      And there should be no translation errors

  Scenario: Jigs UI Drill Down Test
    Given I am on the "jigs" page
    When I click on the "noop" link
    Then I should see a heading "noop"
      And there should be no translation errors

  Scenario: Jigs UI Drill Down Script
    Given I am on the "jigs" page
    When I click on the "script" link
    Then I should see a heading "script"
      And there should be no translation errors
    