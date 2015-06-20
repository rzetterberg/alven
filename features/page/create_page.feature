Feature: Create page
  
  @todo
  Scenario: Create page with used slug
    Given a page with slug "test", title "Test" and content "Testtest" exists
    When I try to save a new page with slug "test"
    Then I should see a duplicate slug error
    
  @todo
  Scenario: Create page with invalid data
    When I try save a page with at least one field of invalid data
    Then I should see an error message under each invalid field

  @todo
  Scenario: Create page with valid data
    When I try to save a new page with slug "test1", title "Test1" and content "Testtest1"
    Then I should see a success message
