from behave import *
 
@given(u'I am authorized as user A')
def step_impl(context):
    b = context.browser

    context.open_url("auth/login")

    b.select_form(nr = 0)

    b.form["email"] = "tester@test.com"
    b.form["password"] = "devpass"

    b.submit()

@when(u'I visit any page in the admin interface')
def step_impl(context):
    context.open_url("admin")

@then(u'I should see the requested page')
def step_impl(context):
    soup = context.get_soup()

    assert soup.title.string == "Admin"

@given(u'I am not authorized as a user')
def step_impl(context):
    context.open_url("auth/logout")

@then(u'I should see the login page')
def step_impl(context):
    soup = context.get_soup()

    assert soup.title.string == "Login"
