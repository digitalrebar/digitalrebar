Feature: Groups
  In order organize resources
  The system operator, Oscar
  wants to be able to put things into groups
  Note: the group code is reflected in "group_cb.erl"

  Scenario: Group List
    Given REST creates a {object:group} "nodelisttest"
    When REST gets the {object:group} list
    Then the list should have an object with key "name" value "nodelisttest"
      And the list should have an object with key "name" value "bddthings"
    Finally REST removes {object:group} "nodelisttest"
    
  Scenario: Group Basic
    When REST gets the {object:group} "bddthings"
    Then the {object:group} is properly formatted
      And there should be a key "category"

  Scenario: Group Add UI category
    Given there is a "ui" group "simpleadd1"
    When REST gets the {object:group} "simpleadd1"
    Then the {object:group} is properly formatted
    Finally REST removes {object:group} "simpleadd1"

  Scenario: Group Add Rack category
    Given there is a "rack" group "simpleadd2"
    When REST gets the {object:group} "simpleadd2"
    Then the {object:group} is properly formatted
    Finally REST removes {object:group} "simpleadd2"

  Scenario: Group Add Tag category
    Given there is a "tag" group "simpleadd3"
    When REST gets the {object:group} "simpleadd3"
    Then the {object:group} is properly formatted
    Finally REST removes {object:group} "simpleadd3"

  Scenario: Group Delete
    Given there is a "ui" group "simpledelete"
    When REST deletes the {object:group} "simpledelete"
    Then there is not a "ui" group "simpledelete"

  Scenario: Add Node to Group
    Skip TODO ZEHICLE disable during refactoring
    Given REST creates a {object:node} "group1.add.test"
      And there is a "ui" group "add2me"
    When REST adds the node "group1.add.test" to "add2me"
    Then the node "group1.add.test" should be in group "add2me"
      And the group "add2me" should have at least "1" node
    Finally REST removes {object:node} "group1.add.test"
      And REST removes {object:group} "add2me"
      
  Scenario: Delete Node from Group
    Skip TODO ZEHICLE disable during refactoring
    Given REST adds the node "group1.node.test" to "bdddelete"
    When REST removes the node "group1.node.test" from "bdddelete"
    Then the node "group1.node.test" should not be in group "bdddelete"
      And the group "bdddelete" should have "0" nodes
    
  Scenario: Move Node between Groups
    Skip this functionality is not current in the API
    Given there is a "ui" group "end1there"
      And REST adds the node "group1.node.test" to "bddthings"
    When REST moves the node "group1.node.test" from "bddthings" to "end1there"
    Then the node "group1.node.test" should not be in group "bddthings"
      And the node "group1.node.test" should be in group "end1there"
    Finally REST removes the node "group1.node.test" from "end1there"
      And REST removes {object:group} "end1there"
