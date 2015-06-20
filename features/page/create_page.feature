Feature: Create page
  
  Background:
    Given a page with slug "test", title "Test" and content "Testtest" exists
  
  @todo
  Scenario: Create page with unused slug
    When I try to save a page with a unique slug
    Then I should see a success message
    
  @todo
  Scenario: Create page with invalid data
    When I try save a page with at least one field of invalid data
    Then I should see an error message under each invalid field
