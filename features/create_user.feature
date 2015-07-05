@admin
Feature: Create user
  As a user I want to be able to create new users so that I can give access to
  other persons without having to share my own credentials.
  
  Background:
    Given I am authorized
  
  @todo
  Scenario: Create a new user
    Given a user with email "new@test.com" does not exist
    When I create a user with email "new@test.com"
    Then I should see the user list with the user "new@test.com"
