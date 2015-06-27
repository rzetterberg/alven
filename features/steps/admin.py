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

# ==============================================================================
# General
 
@given(u'I am authorized')
def step_impl(context):
    login(context)

@given(u'I am not authorized')
def step_impl(context):
    b = context.browser

    context.open_url("admin")

    soup = context.get_soup()

    if soup.title.string != "Admin":
        return

    b.select_form(name = "logout")
    b.submit()

@given(u'a page with slug "{slug}" does not exist')
def step_impl(context, slug):
    curs = context.db.cursor()

    curs.execute("DELETE FROM text_page WHERE permalink = '%s';" % slug)

    context.db.commit()
    curs.close()

@when(u'I visit "{path}"')
def step_impl(context, path):
    context.open_url(path)

@then(u'I should see the "{page_title}" page')
def step_impl(context, page_title):
    soup = context.get_soup()

    assert soup.title.string == page_title

# ==============================================================================
# User related

@when(u'I login')
def step_impl(context):
    login(context)

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

@when(u'I press delete on a page with slug "{slug}"')
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

@when(u'I press confirm')
def step_impl(context):
    b = context.browser

    b.select_form(name = "page_remove")
    b.submit()

@then(u'I should see the page list with the page "{slug}"')
def step_impl(context, slug):
    b = context.browser

    context.open_url("page")

    soup  = context.get_soup()
    table = soup.find("table")

    assert page_exists(table, slug)

@then(u'I should see the page list without the page "{slug}"')
def step_impl(context, slug):
    b = context.browser

    context.open_url("page")

    soup  = context.get_soup()
    table = soup.find("table")

    assert not page_exists(table, slug)

@then(u'I should see a duplicate error message')
def step_impl(context):
    soup = context.get_soup()

    alert = soup.find("div", {"id" : "alert-holder"})

    assert "exists already" in alert.text
