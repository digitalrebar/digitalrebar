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
      And I should see "ipmi"
      And I should see "wsman"
      And I should see "raid-hammer"
      And there should be no translation errors

  Scenario: Available Hammers UI Drill Down Test
    Given I am on the "available_hammers" page
    When I click on the "ssh" link
    Then I should see a heading "ssh"
      And I should see "SecureShellHammer"
      And I should see heading {bdd:crowbar.i18n.available_hammers.show.hammers}
      And there should be no translation errors
