@admin
Feature: Login to admin interface
  
  @todo
  Scenario: Admin start page after login
    Given I am not authorized
    When I login
    Then I should see the "Admin" page
