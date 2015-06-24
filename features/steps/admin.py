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

# ==============================================================================
# General
 
@given(u'I am authorized as the test user')
def step_impl(context):
    b = context.browser

    context.open_url("auth/login")

    b.select_form(nr = 0)

    b.form["email"] = "tester@test.com"
    b.form["password"] = "devpass"

    b.submit()

@given(u'I am not authorized')
def step_impl(context):
    b = context.browser

    context.open_url("admin")

    b.select_form(name = "logout")
    b.submit()

@given(u'a page with slug "{slug}" does not exist')
def step_impl(context, slug):
    pass

# ==============================================================================
# Access related

@when(u'I visit "{path}"')
def step_impl(context, path):
    context.open_url(path)

@then(u'I should see the "{page_title}" page')
def step_impl(context, page_title):
    soup = context.get_soup()

    assert soup.title.string == page_title

# ==============================================================================
# Page related

@when(u'I create a page with slug "{slug}"')
def step_impl(context, slug):
    b = context.browser

    context.open_url("page/create")

    b.select_form(name = "page")

    fields = context.get_hident_fields()

    context.assign_hident(fields, "name", slug)
    context.assign_hident(fields, "permalink", slug)
    context.assign_hident(fields, "body", "Is this my body?")
    context.assign_hident(fields, "is_public", ["yes"])

    b.submit()
@then(u'I should see the page list with the page "{slug}" among the pages')
def step_impl(context, slug):
    b = context.browser

    context.open_url("page")

    soup  = context.get_soup()
    table = soup.find("table")

    assert page_exists(table, slug)
