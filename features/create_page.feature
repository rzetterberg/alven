@admin
Feature: Create page
  As a user I want to be able to create new pages so that I can extend the
  content on my website
  
  Background:
    Given I am authorized
  
  Scenario: Create a new page
    Given a page with slug "page_a" does not exist
    When I create a public page with slug "page_a"
    Then I should see the page list with the page "page_a"

  Scenario: Create a new duplicate page
    Given a page with slug "page_a" does not exist
    When I create a public page with slug "page_a"
    And I create a public page with slug "page_a"
    Then I should see a duplicate error message
