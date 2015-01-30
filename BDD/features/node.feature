Feature: Nodes
  In order check out the nodes
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Admin Node working & defaults
    When REST gets the {object:node} {lookup:crowbar.node_name}
    Then the {object:node} is properly formatted
      And key "deployment_id" should be "1"
      And key "admin" should be "true"
      And key "alive" should be "true"
      And key "available" should be "true"

  Scenario: UI Node List
    When I go to the "nodes" page
    Then I should see {bdd:crowbar.i18n.nodes.index.title}
      And I should see {lookup:crowbar.node_name}
      And there should be no translation errors

  Scenario: UI Node List Click to Node
    Given I am on the "nodes" page
    When I click on the {lookup:crowbar.node_name} link
    Then I should see {lookup:crowbar.node_name}
      And there should be no translation errors

  Scenario: Nodes List
    When REST gets the {object:node} list
    Then the list should have an object with key "name" value {lookup:node.name}

  Scenario: REST JSON check
    When REST gets the {object:node} {lookup:node.name}
    Then the {object:node} is properly formatted
    
  Scenario: REST Can Delete
    Given REST creates the {object:node} "going.going.gone"
      And there are no pending Crowbar runs for {o:node} "going.going.gone"
    When REST deletes the {object:node} "going.going.gone"
    Then I get a {integer:200} result
      And there are no pending Crowbar runs for {o:node} "going.going.gone"
      And there is not a {object:node} "going.going.gone"

  Scenario: REST Can Delete Committed
    Given REST creates and commits the {object:node} "going.committed.gone"
      And there are no pending Crowbar runs for {o:node} "going.committed.gone"
    When REST deletes the {object:node} "going.committed.gone"
    Then I get a {integer:200} result
      And there are no pending Crowbar runs for {o:node} "going.committed.gone"
      And there is not a {object:node} "going.committed.gone"
  
  Scenario: REST Get 404
    When REST gets the {object:node} "thisdoesnotexist"
    Then I get a {integer:404} error
    
  Scenario: Node List
    Given there is a {object:node} "bdd-node-list.example.com"
    When REST gets the {object:node} list
    Then the list should have an object with key "name" value "bdd-node-list.example.com"
      And the list should have an object with key "name" value "bdd1.example.com"
      And the list should have an object with key "name" value {lookup:crowbar.node_name}
    Finally REST removes the {object:node} "bdd-node-list.example.com"
    
  Scenario: Node Page Checkout
    Given there is a {object:node} "bdd-node-page.example.com" marked alive
    When I go to the "nodes/bdd-node-page.example.com" page 
    Then I should see {bdd:crowbar.i18n.common.bootenv.local}
      And I should not see {bdd:crowbar.i18n.common.bootenv.sledgehammer}
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-node-page.example.com"

  Scenario: Node Alive Settable Default False
    Given there is a {object:node} "bdd-alive-false.example.com"
    When REST gets the {object:node} "bdd-alive-false.example.com"
    Then key "alive" should be "true"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-alive-false.example.com"

  Scenario: Node Alive Settable
    Given there is a {object:node} "bdd-alive-set.example.com"
    When REST sets the {object:node} "bdd-alive-set.example.com" {atom:alive} state to be "true"
    Then key "alive" should be "true"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-alive-set.example.com"

  Scenario: Node Available Settable Default False
    Given there is a {object:node} "bdd-available-false.example.com"
    When REST gets the {object:node} "bdd-available-false.example.com"
    Then key "available" should be "false"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-available-false.example.com"

  Scenario: Node into New Deployment
    Given there is a {object:node} "node-deploy.example.com"
      And there is a {object:deployment} "new_deploy"
    When REST sets {object:node} "node-deploy.example.com" property "deployment" to "new_deploy"
    Then the {object:node} is properly formatted
      And key "deployment_id" should not be "1"
    Finally REST removes the {object:node} "node-deploy.example.com"
      And REST removes the {object:deployment} "new_deploy"

  Scenario: Node Available Settable
    Given there is a {object:node} "bdd-available-set.example.com"
    When REST sets the {object:node} "bdd-available-set.example.com" {atom:available} state to be "false"
    Then key "available" should be "false"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-available-set.example.com"

  Scenario: Node UI shows alive
    Given there is a {object:node} "bdd-alive-ui.example.com"
    When I go to the "nodes/bdd-alive-ui.example.com" page 
    Then I should see {bdd:crowbar.i18n.common.state.alive}
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-alive-ui.example.com"

  Scenario: Nodes UI shows alive
    Given there is a {object:node} "bdd-alive-ui.example.com"
    When I go to the "nodes" page 
    Then I should see {bdd:crowbar.i18n.common.state.alive}
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-alive-ui.example.com"

  Scenario: Node UI shows reserved
    Given there is a {object:node} "bdd-reserved-ui.example.com"
      And REST sets the {object:node} "bdd-reserved-ui.example.com" {atom:available} state to be "false"
    When I go to the "nodes/bdd-reserved-ui.example.com" page 
    Then I should see {bdd:crowbar.i18n.common.state.reserved}
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-reserved-ui.example.com"

  Scenario: Nodes UI shows reserved
    Given there is a {object:node} "bdd-reserved-ui1.example.com"
      And REST sets the {object:node} "bdd-reserved-ui1.example.com" {atom:available} state to be "false"
    When I go to the "nodes" page 
    Then I should see {bdd:crowbar.i18n.common.state.reserved}
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-reserved-ui1.example.com"

  Scenario: Node special API for deployment change
    Given there is a {object:node} "bdd-deployment-change.example.com"
      And there is a {object:deployment} "bdd_test1"
    When REST updates an object at "/api/v2/nodes/bdd-deployment-change.example.com" with "{\"deployment\":\"bdd_test1\"}"
    Then key "deployment_id" should not be "1"
      And key "deployment_id" should match id for {object:deployment}
    Finally REST removes the {object:node} "bdd-deployment-change.example.com"
      And REST removes the {object:deployment} "bdd_test1"

  Scenario: Node loads test data
    Unless travis
    Given there is a {object:node} "bdd-discovery.data.edu"
      Given test loads the "node_discovery" data into {object:node} "bdd-discovery.data.edu"
    When REST gets the {object:node} "bdd-discovery.data.edu"
    Then key "discovery" should contain at least "1" items
    Finally REST removes the {object:node} "bdd-discovery.data.edu"

  Scenario: Node takes hint about IP address
    Skip REFACTORING hints
    Given there is a {object:node} "bdd-hint-ip1.data.edu" hinted "ip" as "192.168.124.124"
    When REST gets the {object:node} "bdd-hint-ip1.data.edu"
    Then key "hint" should have json "network-admin:v4addr" with value "192.168.124.124" 
    Finally REST removes the {object:node} "bdd-hint-ip1.data.edu"

  Scenario: Node takes hint about MAC address
    Skip REFACTORING hints
    Given there is a hint "ip" with "192.168.124.124"
      And there is a hint "mac" with "f1:f2:f3:f4:f5:f6"
      And there is a {object:node} "bdd-hint-ip3.data.edu" hinted
    When REST gets the {object:node} "bdd-hint-ip3.data.edu"
    Then key "hint" should have json "network-admin:v4addr" with value "192.168.124.124" 
      And key "hint" should have json "admin_mac" with value "f1:f2:f3:f4:f5:f6" 
    Finally REST removes the {object:node} "bdd-hint-ip3.data.edu"

  Scenario: Provisioner DHCP database uses hint about MAC address
    Skip REFACTORING hints
    Given there is a hint "ip" with "192.168.124.127"
      And there is a hint "mac" with "f6:f5:f4:f3:f2:f1"
      And there is a {object:node} "bdd-hint-ip4.data.edu" hinted
      And process "delayed" returns "delayed_job.([0..9])"
      And there are no pending Crowbar runs for {o:node} {lookup:crowbar.node_name}
      And there are no pending Crowbar runs for {o:node} "bdd-hint-ip4.data.edu"
    When REST requests the "/api/v2/dhcp/bdd-hint-ip4.data.edu" page
    Then Array key "mac_addresses" matches "f6:f5:f4:f3:f2:f1"
      And Array key "v4addr" matches "192.168.124.\\d{1,3}\/\\d{1,2}"
    Finally there are no pending Crowbar runs for {o:node} "bdd-hint-ip4.data.edu"
      And REST removes the {object:node} "bdd-hint-ip4.data.edu"
