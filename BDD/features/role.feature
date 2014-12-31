Feature: Role
  In order to track system roles
  The system operator, Oscar
  wants to be able to manage roles

  Scenario: REST List
    When REST gets the {object:role} list
    Then the page returns {integer:200}
  
  Scenario: REST JSON check
    When REST gets the {object:role} "test-admin"
    Then the {object:role} is properly formatted
    
  Scenario: The page renders
    Given I am on the "roles" page
    Then I should see a heading {bdd:crowbar.i18n.roles.index.title}
      And I should see "test-admin"
      And there are no localization errors

  Scenario: Roles UI click to a snapshot
    Given I am on the "roles" page
    When I click on the "test-admin" link
    Then I should see "test-admin"

  Scenario: Role Page renders
    When I go to the "roles/test-admin" page
    Then I should see "test-admin"
      And there are no localization errors

  Scenario: Roles Page Drill to Role
    Given I am on the "roles" page
    When I click on the "test-admin" link
    Then I should see "test-admin"
      And there are no localization errors

  Scenario: Roles Page Drill to Jig
    Given I am on the "roles" page
    When I click on the "test" link
    Then I should see "test"
      And there are no localization errors

  Scenario: Roles Page Drill to Barclamp
    Given I am on the "roles" page
    When I click on the "test-admin" link
    Then I should see "test-admin"
      And there are no localization errors

  Scenario: Role class override specific works
    When I go to the "/roles/test-admin" page
    Then I should see "[BarclampTest::Admin]"

  Scenario: Role class override specific works for test
    When I go to the "/roles/test-client" page
    Then I should see "[BarclampTest::Role]"

  Scenario: Role class override generic works
    Given there is a {object:role} "BDD_generic" in {object:barclamp} "test" for {object:jig} "test"
    When I go to the "/roles/BDD_generic" page
    Then I should see "[BarclampTest::Role]"
    Finally REST removes the {object:role} "BDD_generic"

  Scenario: Role class no-override works
    Given there is a {object:role} "BDD_no_override" in {object:barclamp} "crowbar" for {object:jig} "test"
    When I go to the "/roles/BDD_no_override" page
    Then I should see "[Role]"
    Finally REST removes the {object:role} "BDD_no_override"
