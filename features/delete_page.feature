@admin
Feature: Delete page
  
  As a user I want to be able to delete pages so that obsolete ones doesn't
  remains as clutter.

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
