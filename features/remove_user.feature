@admin
Feature: Remove user
  As a user I want to be able to remove users that I don't want to allow to make changes
  to my website.
  
  Background:
    Given I am authorized
  
  Scenario: Delete a user
    Given a user with email "new@test.com" does not exist
    When I create a user with email "new@test.com"
    And I delete user with email "new@test.com"
    Then I should see the user list without the user "new@test.com"
