Feature: Documentation
  In order to get a license
  The system operator, Oscar
  wants to be able to read the EULA

  Scenario: EULA Works
    When I go to the "docs/eula" page
    Then the page returns {integer:200}
      And I should see "License"
