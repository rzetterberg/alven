@admin
Feature: User accesses admin interface
  As a user I want to be able to access the admin interface only when I am
  logged in, so that only persons with authorized accounts can make changes to
  my website.
  
  Scenario Outline: Authorized users have access
    Given I am authorized
    When I visit "<path>"
    Then I should see the "<identifier>" page
    
    Examples: Main pages
    | path                     | identifier              |
    | /admin                   | admin-index             |
    | /page                    | page-list               |
    | /page/create             | page-create             |
    | /user                    | user-list               |
    | /user/create             | user-create             |
    | /theme                   | theme-index             |
    | /theme/api_reference     | theme-api-reference     |
    | /theme/compability_check | theme-compability-check |

  Scenario Outline: Unauthorized users does not have access
    Given I am not authorized
    When I visit "<path>"
    Then I should see the "login" page

    Examples: Main pages
    | path                     |
    | /admin                   |
    | /page                    |
    | /page/create             |
    | /user                    |
    | /user/create             |
    | /theme                   |
    | /theme/api_reference     |
    | /theme/compability_check |
