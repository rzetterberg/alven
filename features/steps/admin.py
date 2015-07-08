import re
from behave import *

# ==============================================================================
# Helpers

def page_exists(table, slug):
    return get_page_row(table, slug) is not None

def get_page_row(table, slug):
    tbody = table.find("tbody")
    rows  = tbody.find_all("tr")

    for row in rows:
        cols      = row.find_all("td")
        view_link = cols[2].find("a")
        view_url  = view_link["href"]
        url_parts = view_url.split("/")
        page_slug = url_parts[len(url_parts) - 1]

        if page_slug == slug:
            return row

    return None

def user_exists(table, email):
    return get_user_row(table, email) is not None

def get_user_row(table, email):
    tbody = table.find("tbody")
    rows  = tbody.find_all("tr")

    for row in rows:
        cols       = row.find_all("td")
        curr_email = cols[1]

        if curr_email.text == email:
            return row

    return None

def login(context):
    b = context.browser

    context.open_url("auth/login")

    soup = context.get_soup()

    if soup.title.string == "Admin":
        return

    b.select_form(nr = 0)

    b.form["email"] = "tester@test.com"
    b.form["password"] = "devpass"

    b.submit()

def logout(context):
    b = context.browser

    context.open_url("admin")

    soup = context.get_soup()

    if soup.title.string != "Admin":
        return

    b.select_form(name = "logout")
    b.submit()

# ==============================================================================
# General
 
@given(u'I am authorized')
def step_impl(context):
    login(context)

@given(u'I am not authorized')
def step_impl(context):
    logout(context)

@when(u'I logout')
def step_impl(context):
    logout(context)

@given(u'a page with slug "{slug}" does not exist')
def step_impl(context, slug):
    curs = context.db.cursor()

    curs.execute("DELETE FROM text_page WHERE permalink = '%s';" % slug)

    context.db.commit()
    curs.close()

@given(u'a user with email "{email}" does not exist')
def step_impl(context, email):
    curs = context.db.cursor()

    curs.execute("DELETE FROM public.user WHERE email = '%s';" % email)

    context.db.commit()
    curs.close()

@when(u'I visit "{path}"')
def step_impl(context, path):
    context.open_url(path)

@then(u'I should see the "{page_identifier}" page')
def step_impl(context, page_identifier):
    soup = context.get_soup()

    wrapper = soup.find(
        "div",
        {
            "id" : re.compile(r'.*' + page_identifier + '.*')
        }
    )

    assert wrapper is not None

# ==============================================================================
# User related

@when(u'I login')
def step_impl(context):
    login(context)

@when(u'I login with wrong credentials')
def step_impl(context):
    b = context.browser

    context.open_url("auth/login")

    soup = context.get_soup()

    assert soup.title.string == "Login"

    b.select_form(nr = 0)

    b.form["email"] = "blablabla@asd.com"
    b.form["password"] = "wrongpassword"

    b.submit()

@when(u'I create a user with email "{email}"')
def step_impl(context, email):
    b = context.browser

    context.open_url("user/create")

    b.select_form(name = "user_create")

    fields = context.get_hident_fields()

    context.assign_hident(fields, "email", email)
    context.assign_hident(fields, "is_admin", ["no"])

    b.submit()

@when(u'I delete user with email "{email}"')
def step_impl(context, email):
    b = context.browser

    context.open_url("user")

    soup        = context.get_soup()
    table       = soup.find("table")
    row         = get_user_row(table, email)
    cols        = row.find_all("td")
    remove_link = cols[4].find("form")
    remove_url  = remove_link["action"]

    for form in b.forms():
        if form.action == remove_url:
            b.form = form
            b.submit()

    b.select_form(name = "user_remove")
    b.submit()

@then(u'I should see the user list {inclusion} the user "{email}"')
def step_impl(context, inclusion, email):
    b = context.browser

    context.open_url("user")

    soup  = context.get_soup()
    table = soup.find("table")

    if inclusion == "with":
        assert user_exists(table, email)
    else:
        assert not user_exists(table, email)

# ==============================================================================
# Page related

@when(u'I create a {visibility} page with slug "{slug}"')
def step_impl(context, visibility, slug):
    b = context.browser

    context.open_url("page/create")

    b.select_form(name = "page")

    fields = context.get_hident_fields()

    context.assign_hident(fields, "name", slug)
    context.assign_hident(fields, "permalink", slug)
    context.assign_hident(fields, "body", "Is this my body?")

    if visibility == "public":
        context.assign_hident(fields, "is_public", ["yes"])
    else:
        context.assign_hident(fields, "is_public", ["no"])

    b.submit()

@when(u'I delete page with slug "{slug}"')
def step_impl(context, slug):
    b = context.browser

    context.open_url("page")

    soup        = context.get_soup()
    table       = soup.find("table")
    row         = get_page_row(table, slug)
    cols        = row.find_all("td")
    remove_link = cols[4].find("form")
    remove_url  = remove_link["action"]

    for form in b.forms():
        if form.action == remove_url:
            b.form = form
            b.submit()

    b.select_form(name = "page_remove")
    b.submit()

@then(u'I should see the page list {inclusion} the page "{slug}"')
def step_impl(context, inclusion, slug):
    b = context.browser

    context.open_url("page")

    soup  = context.get_soup()
    table = soup.find("table")

    if inclusion == "with":
        assert page_exists(table, slug)
    else:
        assert not page_exists(table, slug)

@then(u'I should see a duplicate error message')
def step_impl(context):
    soup = context.get_soup()

    alert = soup.find("div", {"id" : "alert-holder"})

    assert "exists already" in alert.text
