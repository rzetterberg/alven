@admin
Feature: User accesses admin interface
  As a user I want to be able to access the admin interface only when I am
  logged in, so that only persons with authorized accounts can make changes to
  my website.
  
  Scenario Outline: Authorized users have access
    Given I am authorized
    When I visit "<path>"
    Then I should see the "<title>" page
    
    Examples: Main pages
    | path         | title       |
    | /admin       | Admin       |
    | /page        | Pages       |
    | /page/create | Create page |
    | /user        | Users       |
    | /user/create | Create user |

  Scenario Outline: Unauthorized users does not have access
    Given I am not authorized
    When I visit "<path>"
    Then I should see the "Login" page

    Examples: Main pages
    | path         |
    | /admin       |
    | /page        |
    | /page/create |
    | /user        |
    | /user/create |
