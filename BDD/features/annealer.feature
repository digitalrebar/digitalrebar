Feature: Rebar Engine
  In order to use run deployment orchestration
  The system operator, Oscar
  wants step through deployment runs

  Scenario: Annealer Page Renders
    When I go to the "annealer" page
    Then I should see a heading {bdd:rebar.i18n.node_roles.anneal.title}
      And I should see {bdd:rebar.i18n.common.state.todo}
      And I should see {bdd:rebar.i18n.common.state.error}
      And I should see {bdd:rebar.i18n.common.state.transition}
      And I should see {bdd:rebar.i18n.common.state.blocked}
      And there should be no translation errors

  Scenario: Add node into Test Deployment
    Given there is a {o:deployment} "bdd_add_node"
      And there is a {o:node} "bdd-add-me.cr0wbar.com"
      And {o:deployment} "bdd_add_node" includes {o:role} "test-event"
    When I add {o:node} "bdd-add-me.cr0wbar.com" to {o:deployment} "bdd_add_node" in {o:role} "test-event"
    Then the {o:node_role} is properly formatted
      And key "state" is {apply:rebar.state.proposed}
    Finally there are no pending Rebar runs for {o:node} "bdd-add-me.cr0wbar.com"
      And REST deletes the {o:deployment} "bdd_add_node"
      And REST deletes the {o:node} "bdd-add-me.cr0wbar.com"

  Scenario: Heartbeat works
    When REST requests the "api/status/heartbeat" page
    Then I get a {integer:200} result

  Scenario: Heartbeat Format
    When REST requests the "api/status/heartbeat" page
    Then there should be a key "active"
    Then there should be a key "todo"
    Then there should be a key "error"