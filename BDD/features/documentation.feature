Feature: Documentation
  EULA page should work
  
  Scenario: EULA Works
    When I go to the "docs/eula" page
    Then the page returns {integer:200}
      And I should see "License"
