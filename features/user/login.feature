@admin
Feature: User logs in
  
  Background:
    Given user A with email "test@tester.com" and password "tester" exists
  
  @todo
  Scenario: Unauthenticated user logs in
    Given I am not authenticated
    When I try to login using user A credentials
    Then I should see a "login successfull" message on the admin index page

  @todo
  Scenario: Authenticated visits login page
    Given I am authenticated as user A
    When I visit the login page
    Then I should be redirected to the admin index page
