@admin
Feature: User logs out
  
  As a user I want to be able to log out from the admin interface when I'm 
  done using it, so that no unauthorized person can edit my website on my 
  computer.
  
  Background:
    Given user A exists
  
  @todo
  Scenario: Authorized user logs out
    Given I am authorized as user A
    When I press the log out button
    Then I should see a "logout successfull" message on the login page
