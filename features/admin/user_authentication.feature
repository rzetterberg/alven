Feature: User authentication
  
  @todo
  Scenario: User logs in
    Given user A with email E and password P exists in the database
    When I try to login using credentials E and P on the login page
    Then I should be authorized as user A and redirected to the admin start page
    
  @todo
  Scenario: User logs out
    Given I am authorized as user A
    When I press the log out button
    Then I should deauthorized and redirected to the login page
  
  @todo
  Scenario: Authorized user visits admin interface
    Given I am authorized as user A
    When I visit any page in the admin interface
    Then I should be allowed to view the page

  @todo
  Scenario: Unauthorized user visits admin interface
    Given I am not authorized as a user
    When I visit any page in the admin interface
    Then I should be redirected to the login page with a "access denied" message
