Feature: User accesses admin interface
  
  Background:
    Given user A exists
  
  @todo
  Scenario: Authorized user visits admin interface
    Given I am authorized as user A
    When I visit any page in the admin interface
    Then I should see the requested page

  @todo
  Scenario: Unauthorized user visits admin interface
    Given I am not authorized as a user
    When I visit any page in the admin interface
    Then I should see a "authorization needed" message on the login page
