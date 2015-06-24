@admin
Feature: Create page
  As a user I want to be able to create new pages so that I can extend the
  content on my website
  
  @todo
  Scenario: Create a new page
    Given I am authorized
    When I create a page with slug "page_a"
    Then I should see the page list with the page "page_a" among the pages
