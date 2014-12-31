Feature: Network Ranges
  In order to manage networks, Operator
  wants to be able to add ranges

  Scenario: REST Range List
    When REST gets the {object:range} list
    Then the page returns {integer:200}

  Scenario: REST JSON check
    When REST requests the "api/v2/networks/admin/network_ranges/admin" page
    Then the {object:networkrange} is properly formatted
  