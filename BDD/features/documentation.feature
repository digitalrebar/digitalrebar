Feature: Documentation
  In order to learn how to use the system
  The system operator, Oscar
  wants to be able to read about the system in the documentation

  Scenario: Doc Index
    When I go to the "docs" page
    Then I should see heading {bdd:crowbar.i18n.docs.index.title}
      And there should be no translation errors

  Scenario: Doc Topic
    Given I am on the "docs" page
    When I click on the "User Guide" link
    Then I should see heading "User Guide"
      And there should be no translation errors

  Scenario: Doc Topic Navigation
    When I go to the "docs/development-guides" page
    Then I should see heading "Development Guide"
      And I should see heading {bdd:crowbar.i18n.docs.show.children}
      And there should be no translation errors

  Scenario: Doc Sub Topic return to Main
    Skip TODO ZEHICLE disable during refactoring
    Given parameter "rebuild" is "false"
    Given I am on the "docs/topic/framework/userguide" page with parameter "rebuild"
    When I click on the "System Documentation \\\(Master Index\\\)" link
    Then I should see "System Documentation"
      And there should be no translation errors

  Scenario: Doc Export 
    Skip TODO ZEHICLE disable during refactoring
    Given parameter "rebuild" is "false"
    Given I am on the "docs/topic/framework/userguide" page with parameter "rebuild"
    When I click on the "Export" link
    Then I should see "Crowbar User Guide"
      And I should see a link to "&lt; Go Back"
      And there should be no translation errors

  Scenario: EULA Works
    When I go to the "docs/eula" page
    Then the page returns {integer:200}
      And I should see "License"
