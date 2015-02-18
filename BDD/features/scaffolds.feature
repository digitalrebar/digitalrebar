Feature: Scaffolds
  In order develop the system
  The devoper operator, Greg
  wants to be able to quickly check the models
  
  Scenario: Attribs
    When I go to the "utils/scaffolds/attribs?limit=1" page
    Then I should see heading "Attribs"
      And I should see "Name"
      And I should see "Map"
      And I should see "Type"
      And there should be no translation errors

  Scenario: Barclamp
    When I go to the "utils/scaffolds/barclamps?limit=1" page
    Then I should see heading "Barclamps"
      And I should see "Name"
      And I should see "Description"
      And I should see "Display"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors
      
  Scenario: Nodes
    When I go to the "utils/scaffolds/nodes?limit=1" page
    Then I should see heading "Nodes"
      And I should see "Name"
      And I should see "Description"
      And I should see "Groups"
      And I should see "Order"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors

  Scenario: Roles
    When I go to the "utils/scaffolds/roles?limit=1" page
    Then I should see heading "Roles"
      And I should see "Description"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors    

Scenario: NodeRoles
    When I go to the "utils/scaffolds/node_roles?limit=1" page
    Then I should see heading "NodeRoles"
      And I should see "Node"
      And I should see "Role"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors

Scenario: NodeRoleAttribLinks
    When I go to the "utils/scaffolds/node_role_attrib_links?limit=1" page
    Then I should see heading "NodeRoleAttribLinks"
      And I should see "Attrib"
      And I should see "Parent"
      And I should see "Child"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors

Scenario: Hammers
  When I go to the "utils/scaffolds/hammers?limit=1" page
  Then I should see heading "Hammers"
    And I should see "Name"
    And I should see "Authenticator"
    And I should see "Available Hammer"
    And I should see "Node"
    And there should be no translation errors   

Scenario: Available Hammers
  When I go to the "utils/scaffolds/available_hammers?limit=1" page
  Then I should see heading "AvailableHammers"
    And I should see "Name"
    And I should see "Klass"
    And I should see "Hammers"
    And I should see "Priority"
    And there should be no translation errors   

  Scenario: Runs
    When I go to the "utils/scaffolds/runs?limit=1" page
    Then I should see heading "Runs"
      And I should see "Running"
      And I should see "Node"
      And I should see "Node role"
      And there should be no translation errors   

  Scenario: Navs
    When I go to the "utils/scaffolds/navs?limit=1" page
    Then I should see heading "Navs"
      And I should see "Name"
      And I should see "Description"
      And I should see "Development"
      And I should see "Parent item"
      And I should see "Path"
      And I should see "Order"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
      
  Scenario: Docs
    When I go to the "utils/scaffolds/docs?limit=1" page
    Then I should see heading "Docs"
      And I should see "Description"
      And I should see "Order"
      And I should see "Parent"
      And I should see "Children"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
      
  Scenario: Deployments
    When I go to the "utils/scaffolds/deployments?limit=1" page
    Then I should see heading "Deployments"
      And I should see "Name"
      And I should see "Description"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors 
                  
  Scenario: Jigs
    When I go to the "utils/scaffolds/jigs?limit=1" page
    Then I should see heading "Jigs"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors 
            
  Scenario: Groups
    When I go to the "utils/scaffolds/groups?limit=1" page
    Then I should see heading "Groups"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Category"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
            
