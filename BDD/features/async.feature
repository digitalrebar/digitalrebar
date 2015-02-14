Feature: Async
  In order to allow external operations
  The system operator, Oscar
  wants to be able to allow node roles to spin while he works

  Scenario: Test-Async attrib is created
    When REST gets the {object:attrib} "async-result"
    Then the {object:attrib} is properly formatted

  Scenario: Node goes to ready in 1 second
    Given REST creates the {object:node} "base.async.com"
    When after 1 second
    Then {object:node} "base.async.com" should not be in state "transition"
      And {object:node} "base.async.com" should not be in state "todo"
      And {object:node} "base.async.com" should be in state "active"
    Finally REST removes the {object:node} "base.async.com"

  Scenario: Async holds in transition
    Given REST creates the {object:node} "first.async.com"
      And {object:node} "first.async.com" has the "test-async" role
    When {object:node} "first.async.com" is committed
      And after 1 second
    Then {object:node} "first.async.com" should be in state "transition"
      And {object:node} "first.async.com" should not be in state "active"
      And {object:node} "first.async.com" should not be in state "todo"
    Finally REST removes the {object:node} "first.async.com"

  Scenario: Async past transition
    Given REST creates the {object:node} "second.async.com"
      And {object:node} "second.async.com" has the "test-async" role
      And {object:node} "second.async.com" is committed
      And after 1 second
    When REST retries {object:node} "second.async.com" {object:node_role} "test-async"
      And after 1 second
    Then {object:node} "second.async.com" should not be in state "transition"
      And {object:node} "second.async.com" should be in state "active"
      And {object:node} "second.async.com" should not be in state "todo"
    Finally REST removes the {object:node} "second.async.com"
