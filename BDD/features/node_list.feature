Feature: Node List
  In order bulk edit nodes
  The system operator, Oscar
  wants to be able to edit all the nodes on a single page

  Scenario: UI Node List
    When I go to the "dashboard/list" page
    Then I should see {bdd:crowbar.i18n.dashboard.list.title}
      And I should see {lookup:crowbar.node_name}
      And I should see {bdd:crowbar.i18n.all}
      And there should be no translation errors

  Scenario: UI Node List for Deployment
    When I go to the "dashboard/list/system" page
    Then I should see {bdd:crowbar.i18n.dashboard.list.title}
      And I should see {lookup:crowbar.node_name}
      And I should see "system"
      And there should be no translation errors

  Scenario: UI Node List Click to Node
    Given I am on the "dashboard/list" page
    When I click on the {lookup:crowbar.node_name} link
    Then I should see {lookup:crowbar.node_name}
      And there should be no translation errors

  Scenario: Filter based on Deployment
    Given REST creates a {object:node} "bdd-bulk-filter.cr0wbar.com"
      And REST creates a {object:deployment} "bddfilter"
      And REST updates an object at "/api/v2/nodes/bdd-bulk-filter.cr0wbar.com" with "{\"node\":{\"deployment\":\"bddfilter\"}}"
    When I go to the "dashboard/list/bddfilter" page
    Then I should see a link to "bdd-bulk-filter.cr0wbar.com" 
      And I should see "bddfilter"
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-bulk-filter.cr0wbar.com"
      And REST removes the {object:deployment} "bddfilter"

  Scenario: Update Node Information via UI Put
    Given REST creates a {object:node} "bdd-bulk-edit.cr0wbar.com"
      And I put "/dashboard/list?node:bdd-bulk-edit.cr0wbar.com:alias=bdd-bdd-bdd&node:bdd-bulk-edit.cr0wbar.com:description=foobar"
    When REST gets the {object:node} "bdd-bulk-edit.cr0wbar.com"
    Then key "alias" should be "bdd-bdd-bdd"
      And key "alias" should not be "bdd-bulk-edit.cr0wbar.com"
      And key "description" should be "foobar"
    Finally REST removes the {object:node} "bdd-bulk-edit.cr0wbar.com"
