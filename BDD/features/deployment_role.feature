Feature: DeploymentRole
  In order to track system deployed roles
  The system operator, Oscar
  wants to be able to manage deployment roles

  Scenario: REST DeploymentRole List
    When REST gets the {object:deployment_role} list
    Then the page returns {integer:200}
      
  Scenario: The Deployment Role List page renders
    Given I am on the "deployment_roles" page
    Then I should see a heading {bdd:crowbar.i18n.deployment_roles.index.title}
      And there are no localization errors

  Scenario: Add Role into Test Deployment
    Given there is a {o:deployment} "bdd_deployment_role"
    When {o:deployment} "bdd_deployment_role" includes {o:role} "test-event"
    Then the {o:deployment_role} is properly formatted
    Finally REST deletes the {o:deployment} "bdd_deployment_role"

  Scenario: Deployment Role network-server page renders
    Given I am on the "deployment_roles/network-server?deployment=system" page
    Then I should see a heading {bdd:crowbar.i18n.deployment_roles.show.attribs}
      And I should see {bdd:crowbar.i18n.common.roles.network-server} 
      And there are no localization errors

  Scenario: Deployment Role 3 page renders
    Given I am on the "deployment_roles/network-admin?deployment=system" page
    Then I should see a heading {bdd:crowbar.i18n.deployment_roles.show.attribs} 
      And I should see {bdd:crowbar.i18n.common.roles.network-admin}
      And there are no localization errors