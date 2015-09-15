Feature: Support UI
  In order to support the Rebar framework
  The system operator, Oscar
  wants to be able to do administration like export

  Scenario: Localization AJAX CN
    When I18N checks "chuck_norris"
    Then I should see "Die!!!"

  Scenario: Localization AJAX Hit
    When I18N checks "test.verify"
    Then I should see "Affirmative"

  Scenario: Localization AJAX Miss
    When I18N checks "test.miss"
    Then I get a {integer:404} error

  Scenario: Use the Log Marker
    When I go to the "utils/marker/foo" page
    Then I should see "foo"
    
  Scenario: Localization from Regular Step
    When I go to the "barclamps" page
    Then I should see {bdd:rebar.i18n.barclamps.index.title}
    
  Scenario: Find Mark in Log
    While local
    Given I mark the logs with "REMAIN CALM"
    When I inspect the log for "MARK >>>>>"
    Then I should grep "MARK >>>>>"
      And I should grep "REMAIN_CALM"
      And I should grep "<<<<< KRAM"

  Scenario: Settings Page
    When I go to the "utils/settings" page 
    Then I should see a heading {bdd:rebar.i18n.support.settings.title}
      And there are no localization errors
    
  Scenario: Settings Change False
    Given I set {object:user} setting "edge" to "false"
    When I go to the "utils/settings" page
    Then I should see an unchecked check box "edge"

  Scenario: Settings Change True
    Skip this test is broken in Travis
    Given I set {object:user} setting "fast_refresh" to "true"
    When I go to the "utils/settings" page
    Then I should see a checked check box "fast_refresh"

  Scenario: Settings Change not True
    Given I set {object:user} setting "edge" to "foo"
    When I go to the "utils/settings" page
    Then I should see an unchecked check box "edge"
