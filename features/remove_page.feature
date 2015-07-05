@admin
Feature: Remove page
  As a user I want to be able to remove obsolete pages to avoid having a 
  large list of unused pages.
  
  Background:
    Given I am authorized
  
  Scenario: Delete a page
    Given a page with slug "page_a" does not exist
    When I create a public page with slug "page_a"
    And I delete page with slug "page_a"
    Then I should see the page list without the page "page_a"
