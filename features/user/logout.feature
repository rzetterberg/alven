@admin
Feature: User logs out
  
  Background:
    Given user A exists
  
  @todo
  Scenario: Authorized user logs out
    Given I am authorized as user A
    When I press the log out button
    Then I should see a "logout successfull" message on the login page
