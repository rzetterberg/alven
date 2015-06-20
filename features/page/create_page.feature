Feature: Create page
  
  Background:
    Given a page with slug "test", title "Test" and content "Testtest" exists
    And I visit the "create page" page
  
  @todo
  Scenario: Create page with unused slug
    Given I enter slug "new_test", title "Test" and content "Testtest" 
    When I press the save button
    Then I should see a success message and the page list where the new page is added

  @todo
  Scenario: Create page with used slug
    Given I enter slug "test", title "Test" and content "Testtest"
    When I press the save button
    Then I should see an duplicate error message under the slug field
    
  @todo
  Scenario: Create page with invalid data
    Given I enter slug "1!'2qwe", title "!''Test" and content "Testtest"
    When I press the save button
    Then I should see an error message under the invalid fields slug and title
