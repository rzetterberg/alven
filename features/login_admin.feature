@admin
Feature: Login to admin interface
  
  Scenario: Admin start page after login
    Given I am not authorized
    When I login
    Then I should see the "Admin" page

  Scenario: Login page after failed login
    Given I am not authorized
    When I login with wrong credentials
    Then I should see the "Login" page
