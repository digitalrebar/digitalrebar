Feature: Barclamp UI
  In order setup the sytem
  The system operator, Oscar
  wants to be able to select barclamps

  Scenario: Basic Screen
    When I go to the "barclamps" page
    Then I should see a heading {bdd:crowbar.i18n.barclamps.index.title}
      And I should see "crowbar"
      And I should see "test"
      And I should see "network"
      And I should see "deployer"
      And I should see "provisioner"
      And there should be no translation errors
      
  Scenario: Barclamps List
    When REST gets the {object:barclamp} list
    Then the list should have an object with key "name" value "crowbar"
      And the list should have an object with key "name" value "provisioner"
      And the list should have an object with key "name" value "network"

  Scenario: REST JSON check
    When REST gets the {object:barclamp} "crowbar"
    Then the {object:barclamp} is properly formatted
    
  Scenario: REST Cannot Delete
    When REST deletes the {object:barclamp} "crowbar"
    Then I get a {integer:405} error
  
  Scenario: REST Can Update
    Skip Write a scratch barclamp to load
    When REST updates the {object:barclamp} "crowbar"
    Then I get a {integer:405} error

  Scenario: REST Get 404
    When REST gets the {object:barclamp} "thisdoesnotexist"
    Then I get a {integer:404} error
    
  Scenario: REST Can Create
    Skip write a scratch barclamp to load
    When REST creates the {object:barclamp} "foo"
    Then I get a {integer:405} error
