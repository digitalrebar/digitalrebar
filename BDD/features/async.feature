Feature: Async
  In order to allow external operations
  The system operator, Oscar
  wants to be able to allow node roles to spin while he works

  Scenario: Node goes to ready in 1 second
    Given REST creates the {object:node} "base.async.com"
    When I sleep for 1 second
    Then {object:node} "base.async.com" should not be in state "transition"
      And {object:node} "base.async.com" should not be in state "todo"
      And {object:node} "base.async.com" should be in state "ready"
    Finally I remove the {object:node} "base.async.com"

  Scenario: Async holds in transition
    Given REST creates the {object:node} "first.async.com"
    When {object:node} "first.async.com" has the "test-async" role
      And I sleep for 1 second
    Then {object:node} "first.async.com" should be in state "transition"
      And {object:node} "first.async.com" should not be in state "ready"
      And {object:node} "first.async.com" should not be in state "todo"
    Finally I remove the {object:node} "first.async.com"

  Scenario: Async past transition
    Given REST creates the {object:node} "second.async.com"
    When {object:node} "first.async.com" has the "test-async" role
      And I sleep for 1 second
    Then {object:node} "first.async.com" should be in state "transition"
      And {object:node} "first.async.com" should not be in state "ready"
      And {object:node} "first.async.com" should not be in state "todo"
    Finally I remove the {object:node} "first.async.com"
