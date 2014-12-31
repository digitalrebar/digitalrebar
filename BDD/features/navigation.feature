Feature: Navigation, Check Core Navigation
  In order use the system
  The system operator, Oscar
  wants to be able to navigate around
  
  Scenario: Translation Check
    When I go to the home page
    Then there should be no translation errors

  Scenario: Top Nav Renders
    When I go to the home page
    Then I should not see "Render Error"
          
  Scenario: Home Page Nav
    When I go to the home page
    Then I should see a menu for "Barclamps"
      And I should see a menu for "Deployments"
      And I should see a menu for "Nodes"
      And I should see a menu for "Utilities"
      And I should see a menu for "Help"
      And I should see "OpenCrowbar Project Team"
      And I should not see "something went wrong"
      And there should be no translation errors

  Scenario: Nodes Nav
    Given I am on the home page
    When I click on the "Nodes" menu item
    Then I should see a menu for {bdd:crowbar.i18n.nodes.index.title}
      And there should be no translation errors
