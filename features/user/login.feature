@admin
Feature: User logs in
  
  As a user I want to be able to login with my email and password, so that I 
  don't need to remember yet another account name.
  
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
