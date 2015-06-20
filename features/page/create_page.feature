Feature: Create page
  
  Background:
    Given a page with slug "test", title "Test" and content "Testtest" exists
    And I visit the "create page" page
  
  @todo
  Scenario: Create page with unused slug
    When I try to save a page with a unique slug
    Then I should see a success message and the page list where the new page is added
    
  @todo
  Scenario: Create page with invalid data
    When I try save a page with at least on field of invalid data
    Then I should see an error message under each invalid field
