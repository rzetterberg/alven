@admin
Feature: User accesses admin interface
  
  As a user I want to be able to access the admin interface only
  when I am logged in, so that only people with user accounts can
  make changes to my website.
  
  @todo
  Scenario: Authorized users have access
    Given I am authorized as the test user
    When I visit "/admin"
    Then I should see the "Admin" page

  @todo
  Scenario: Unauthorized users does not have access
    Given I am not authorized
    When I visit "/admin"
    Then I should see the "Login" page
