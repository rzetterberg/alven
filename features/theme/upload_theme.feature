@admin
Feature: Upload theme
  
  As a user I want to be able to upload new themes so that I have more designs
  to choose from.
  
  @todo
  Scenario: Upload zip file
    When I try to upload a theme zip file
    Then I should see a success message

  @todo
  Scenario: Upload unknown file
    When I try to upload a file that is not a zip file
    Then I should see a wrong file type message
