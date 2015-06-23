@admin
Feature: Choose theme
  
  As a user I want to be able to change the way my website looks by choosing a
  what theme to be active.
  
  @todo
  Scenario: Change to another theme
    Given theme A with title "Test theme A" exists
    And theme B with title "Test theme B" exists and is the active theme
    When I change active theme to A
    Then I should see a success message
