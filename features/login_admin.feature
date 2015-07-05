@admin
Feature: Login to admin interface
  
  @todo
  Scenario: Admin start page after login
    Given I am not authorized
    When I login
    Then I should see the "Admin" page

  @todo
  Scenario: Login page after failed login
    Given I am not authorized
    When I login with wrong credentials
    Then I should see the "Login" page
