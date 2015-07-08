@admin
Feature: Logout from admin interface
  
  Scenario: Login page after logout
    Given I am not authorized
    When I login
    And I logout
    Then I should see the "login" page
