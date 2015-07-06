@admin
Feature: Login to admin interface
  
  Scenario: Admin start page after login
    Given I am not authorized
    When I login
    Then I should see the "admin-index" page

  Scenario: Login page after failed login
    Given I am not authorized
    When I login with wrong credentials
    Then I should see the "login" page
