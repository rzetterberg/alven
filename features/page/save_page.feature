Feature: Save page
  
  Background:
    Given a page with slug "test", title "Test" and content "Testtest" exists
  
  @todo
  Scenario: Create page with used slug
    When I try to save a new page with slug "test"
    Then I should see a duplicate slug error

  @todo
  Scenario: Edit page with used slug
    Given a page with slug "test2", title "Test2" and content "Testtest2" exists
    When I save page "test2" with the slug changed to "test"
    Then I should see a duplicate slug error
    
  @todo
  Scenario: Save page with invalid data
    When I try save a new or existing page with at least one field of invalid data
    Then I should see an error message under each invalid field

  @todo
  Scenario: Save page with valid data
    When I try to save a new or existing page with valid and unique data
    Then I should see a success message
