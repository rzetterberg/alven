import mechanize, os, re, cookielib, os.path
from bs4 import BeautifulSoup

def get_hident_fields(context):
    soup = context.get_soup()

    field_groups = soup.find_all("div", {"class" : "form-group"})
    fields = {}

    for group in field_groups:
        label = group.find("label")

        if label is None:
            continue

        fname = label.text.lower()
        fname = fname.replace(" ", "_")

        field = group.find("input")
        area  = group.find("textarea")

        if field is not None:
            fields[fname] = field["name"]
        elif area is not None:
            fields[fname] = area["name"]

    return fields

def assign_hident(context, fields, k, v):
    fkey = fields[k]
    context.browser.form[fkey] = v

def before_all(context):  
    context.base_url = os.environ['BUILDER_PORT'].replace("tcp:", "http:")

    b = mechanize.Browser()

    b.set_cookiejar(cookielib.LWPCookieJar())

    b.set_handle_equiv(False)
    b.set_handle_equiv(True)
    b.set_handle_redirect(True)
    b.set_handle_referer(True)
    b.set_handle_robots(False)

    b.set_handle_refresh(
        mechanize._http.HTTPRefreshProcessor(), max_time = 1
    )

    b.addheaders = [
        ('User-agent', 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')
    ]

    context.browser = b

    context.get_soup = lambda: BeautifulSoup(b.response().read())
    context.open_url = lambda p: b.open(os.path.join(context.base_url, p))
    context.get_hident_fields = lambda: get_hident_fields(context)
    context.assign_hident = lambda fs, k, v: assign_hident(context, fs, k, v)
