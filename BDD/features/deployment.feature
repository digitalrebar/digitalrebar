Feature: Deployments
  In order to track system deploymenturation
  The system operator, Oscar
  wants to be able to manage deploymenturations

  Scenario: REST Deployment List
    When REST gets the {object:deployment} list
    Then the page returns {integer:200}
  
  Scenario: REST JSON check
    When REST gets the {object:deployment} "system"
    Then the {object:deployment} is properly formatted
    
  Scenario: The Deployment UI page renders
    Given I am on the "deployments" page
    Then I should see a heading {bdd:crowbar.i18n.deployments.index.title}
      And I should see "system"
      And I should see {apply:crowbar.i18n.deployments.index.parent}
      And there are no localization errors

  Scenario: The system deployment has the system flag true
    When REST gets the {object:deployment} "system"
    Then key "system" should be "true"
      And key "name" should be "system"
      And key "parent_id" should be "null"

  Scenario: Can create new deployment from API
    Given there is not a {object:deployment} "bdd_deploy_test"
    When REST creates the {object:deployment} "bdd_deploy_test"
    Then there is a {object:deployment} "bdd_deploy_test"
    Finally REST removes the {object:deployment} "bdd_deploy_test"

  Scenario: New Deployment renders in UI
    Given there is a {object:deployment} "bdd_deploy_showme"
    When I go to the "deployments/bdd_deploy_showme" page
    Then I should see "bdd_deploy_showme"
      And I should not see "something went wrong"
      And there should be no translation errors
    Finally REST removes the {object:deployment} "bdd_deploy_showme"

  Scenario: New Deployment is child of System
    Given there is a {object:deployment} "bdd_deploy_child"
    When REST gets the {object:deployment} "bdd_deploy_child"
    Then key "parent_id" should be "1"
    Finally REST removes the {object:deployment} "bdd_deploy_child"

  Scenario: Parent Deployment shown on UI
    Given there is a {object:deployment} "bdd_deploy_child_ui"
    When I go to the "deployments/bdd_deploy_child_ui" page
    Then I should see a link to "system"
    Finally REST removes the {object:deployment} "bdd_deploy_child_ui"
  
  Scenario: Validate deployment status
    Given there is a {object:deployment} "status1"
    When REST requests the "/api/status/deployments/status1" page
    Then Array key "id" matches "([\\d])"
      And Array key "status" matches "([proposed])"
      And Array key "state" matches "([0-5])"
      And Array key "md5" is an empty string
      And Array contains key "node_roles"
    Finally REST removes the {object:deployment} "status1"

Scenario: Validate deployment status all
    Skip while ZEHICLE figures out 404 on this page
    When REST requests the "/api/status/deployments/" page
    Then Array key "id" matches "-1"
      And Array key "status" matches "([unknown])"
      And Array key "state" matches "-1"
      And Array key "md5" is an empty string
      And Array contains key "node_roles"

  Scenario: Validate system deployment status
    Given there is a {object:node} "status1"
    When REST requests the "/api/status/deployments/system" page
    Then Array key "id" matches "1"
      And Array key "status" matches "([active|committed])"
      And Array key "state" matches "([0-5])"
      And Array key "md5" matches "([\\da-f]{32})"
      And Array contains key "node_roles"
    Finally REST removes the {object:node} "status1"
