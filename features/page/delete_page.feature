@admin
Feature: Delete page

  Background:
    Given page A with slug "page_a" exists
  
  @todo
  Scenario: Confirm delete page
    When I try to delete page A
    And I confirm the delete prompt
    Then I should see a successful delete message

  @todo
  Scenario: Confirm delete page
    When I try to delete page A
    And I cancel the delete prompt
    Then I be redirected to the page list
