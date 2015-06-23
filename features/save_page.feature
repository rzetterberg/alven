@admin
Feature: Save page
  
  As a user I want to be able to save new and existing pages so that the content
  of the web page can easily be updated/extended.
  
  Background:
    Given page A with slug "page_a" exists
    Given page B with slug "page_b" exists
  
  @todo
  Scenario: Create page with used slug
    When I try to save a new page with slug "page_a"
    Then I should see a duplicate slug error

  @todo
  Scenario: Edit page with used slug
    When I change the slug to "page_a" of page B and save 
    Then I should see a duplicate slug error
    
  @todo
  Scenario: Save page with invalid data
    When I try save a new or existing page with at least one field of invalid data
    Then I should see error messages under each invalid field

  @todo
  Scenario: Save page with valid data
    When I try to save a new or existing page with valid and unique data
    Then I should see a success message
