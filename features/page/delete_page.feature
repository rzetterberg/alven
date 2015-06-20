Feature: Delete page

  Background:
    Given a page with slug "test", title "Test" and content "Testtest" exists
  
  @todo
  Scenario: Delete page
    When I try to delete the "test" page 
    Then I should see a confirm delete prompt

  @todo
  Scenario: Accept confirm delete prompt
    When I accept a delete confirm prompt
    Then I should see a success message

  @todo
  Scenario: Cancel confirm delete prompt 
    When I cancel a delete confirm prompt
    Then I should be redirected to the list page
