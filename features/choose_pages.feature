@admin
Feature: Choose pages
  
  As a user I want to be able to choose what pages should be visible in the 
  navigation menu, so that it is easy to adapt what pages are publically available.
  
  Background:
    Given page A with slug "pagea" and title "Page A" exists
    And page B with slug "pageb" and title "Page B" exists
    And menu contains page A
  
  @todo
  Scenario: Add a page to the menu
    When I add page B to the menu
    Then I should see pages A and B in the menu

  @todo
  Scenario: Remove page from the menu
    When I remove page A from the menu
    Then I should see no pages in the menu
