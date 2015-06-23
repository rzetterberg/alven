from behave import *

@given(u'user A exists')
def step_impl(context):
    pass

@given(u'I am authorized as user A')
def step_impl(context):
    b   = context.browser
    url = context.base_url + "/auth/login"

    b.get(url)

    email = b.find_element_by_name("email")
    email.send_keys("tester@test.com")

    passwd = b.find_element_by_name("password")
    passwd.send_keys("devpass")
    passwd.submit()

@when(u'I visit any page in the admin interface')
def step_impl(context):
    b = context.browser

    b.get("http://localhost:3000/admin")

@then(u'I should see the requested page')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then I should see the requested page')

@given(u'I am not authorized as a user')
def step_impl(context):
    raise NotImplementedError(u'STEP: Given I am not authorized as a user')

@then(u'I should see a "authorization needed" message on the login page')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then I should see a "authorization needed" message on the login page')
