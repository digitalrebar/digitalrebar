Feature: NodeRole
  In order to track system node_roles
  The system operator, Oscar
  wants to be able to manage node_roles

  Scenario: REST List
    When REST gets the {object:node_role} list
    Then the page returns {integer:200}

  Scenario: REST can add a node to a role
    Given there is a {object:deployment} "bdd_deploy_add_role"
      And there is a {object:node} "bdd-add-me-to-role.cr0wbar.com"
    When I add {object:node} "bdd-add-me-to-role.cr0wbar.com" to {object:deployment} "bdd_deploy_add_role" in {object:role} "test-client"
    Then I get a {integer:200} result
      And key "state" should be {apply:rebar.state.proposed}
      And the {object:node_role} is properly formatted
    Finally REST removes the {object:deployment} "bdd_deploy_add_role"
      And REST removes the {object:node} "bdd-add-me-to-role.cr0wbar.com"

  Scenario: The page renders
    When I go to the "node_roles" page
    Then I should see a heading {bdd:rebar.i18n.node_roles.index.title}
      And I should see "system"
      And I should see "rebar-admin-node"
      And there are no localization errors

  Scenario: UI Index drill to deployment
    Given I am on the "node_roles" page
    When I click on the "system" link
    Then I should see "system"
      And there are no localization errors

  Scenario: UI Index drill to role
    Given I am on the "node_roles" page
    When I click on the "rebar-admin-node" link
    Then I should see a heading "rebar-admin-node"
      And there are no localization errors

  Scenario: Node_Error false
    Skip Run refactoring assumes runs can run
    Given REST creates the {object:node} "node.error.false"
      And I add {object:node} "node.error.false" to {object:deployment} "system" in {object:role} "test-client"
      And I add {object:node} "node.error.false" to {object:deployment} "system" in {object:role} "test-library"
      And {object:node} "node.error.false" is committed
    When REST requests the "api/v2/nodes/node.error.false/node_roles/test-client" page
    Then key "node_error" should be "false"
    Finally REST removes the {object:node} ""node.error.false"

  Scenario: Node_Error true
    Skip Run refactoring assumes runs can run
    Given REST creates the {object:node} "node.error.true"
      And I add {object:node} "node.error.true" to {object:deployment} "system" in {object:role} "test-client"
      And I add {object:node} "node.error.true" to {object:deployment} "system" in {object:role} "test-fails"
      And {object:node} "node.error.true" is committed
    When REST requests the "api/v2/nodes/node.error.true/node_roles/test-client" page
    Then key "node_error" should be "true"
