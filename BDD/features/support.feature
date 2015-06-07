Feature: Support UI
  In order to support the Crowbar framework
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
    Then I should see {bdd:crowbar.i18n.barclamps.index.title}
    
  Scenario: Find Mark in Log
    While local
    Given I mark the logs with "REMAIN CALM"
    When I inspect the log for "MARK >>>>>"
    Then I should grep "MARK >>>>>"
      And I should grep "REMAIN_CALM"
      And I should grep "<<<<< KRAM"

  Scenario: Settings Page
    When I go to the "utils/settings" page 
    Then I should see a heading {bdd:crowbar.i18n.support.settings.title}
      And there are no localization errors
    
  Scenario: Settings Change False
    Given I set {object:user} setting "doc_sources" to "false"
    When I go to the "utils/settings" page
    Then I should see an unchecked check box "doc_sources"

  Scenario: Settings Change True
    Given I set {object:user} setting "doc_sources" to "true"
    When I go to the "utils/settings" page
    Then I should see a checked check box "doc_sources"

  Scenario: Settings Change not True
    Given I set {object:user} setting "doc_sources" to "foo"
    When I go to the "utils/settings" page
    Then I should see an unchecked check box "doc_sources"

  Scenario: Settings Change omitted
    Skip until Rob can help figure out what busted here.
    Given I set {object:user} setting "anything_else" to "true"
    When I go to the "utils/settings" page
    Then I should see an unchecked check box "doc_sources"

  Scenario: Settings Change Visible Off
    Skip until Rob can help figure out what busted here.
    Given I set {object:user} setting "debug" to "false"
    When I go to the "docs/development-guides/README.md" page
    Then I should not see heading {bdd:crowbar.i18n.debug}
      And I should see heading "Development Guide"

  Scenario: Settings Change Visible On
    Skip until Rob can help figure out what busted here.
    Given I set {object:user} setting "debug" to "true"
    When I go to the "docs/development-guides/README.md" page
    Then I should see heading {bdd:crowbar.i18n.debug}
      And I should see heading "Development Guide"
