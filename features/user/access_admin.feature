@admin
Feature: User accesses admin interface
  
  As a user I want to be able to access the admin interface only
  when I am logged in, so that only people with user accounts can
  make changes to my website.
  
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
