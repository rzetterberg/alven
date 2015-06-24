import mechanize, os, re, cookielib, os.path, yaml, sys, re, psycopg2
from bs4 import BeautifulSoup

# ==============================================================================
# Helpers

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

def replace_settings_env(conf):
    for key in conf:
        val = conf[key]

        if isinstance(val, dict):
            replace_settings_env(val)
            continue

        if not isinstance(val, str):
            continue

        parts = re.search('^_env:([^:]+):(.+)$', val, re.IGNORECASE)

        if parts is None:
            continue

        env_val = os.environ.get(parts.group(1))

        if env_val is None:
            conf[key] = parts.group(2)
        else:
            conf[key] = env_val

def read_settings():
    f = open("src/config/settings.yml")
    settings = yaml.load(f)
    f.close()

    replace_settings_env(settings)

    settings["approot"] = os.environ['BUILDER_PORT'].replace("tcp:", "http:")

    return settings

def setup_browser():
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

    return b

def reset_db(context):
    curs = context.db.cursor()

    curs.execute("TRUNCATE TABLE text_page RESTART IDENTITY;")

    curs.execute(
        "INSERT INTO text_page " +
        "(name, permalink, body, public) " +
        "VALUES ('Index', 'index', '', true);"
    )

    context.db.commit()
    curs.close()

# ==============================================================================
# Behave hooks

def before_all(context):  
    settings = read_settings()

    s = settings["database"]
    context.db = psycopg2.connect(
        user     = s["user"],
        host     = s["host"],
        database = s["database"],
        password = s["password"]
    )

    reset_db(context)

    context.browser = setup_browser()

    context.get_soup = lambda: BeautifulSoup(context.browser.response().read())
    context.open_url = lambda p: context.browser.open(os.path.join(settings["approot"], p))
    context.get_hident_fields = lambda: get_hident_fields(context)
    context.assign_hident = lambda fs, k, v: assign_hident(context, fs, k, v)

def after_all(context):
    context.db.close()

"""
>>> cur = conn.cursor()

# Execute a command: this creates a new table
>>> cur.execute("CREATE TABLE test (id serial PRIMARY KEY, num integer, data varchar);")

# Pass data to fill a query placeholders and let Psycopg perform
# the correct conversion (no more SQL injections!)
>>> cur.execute("INSERT INTO test (num, data) VALUES (%s, %s)",
...      (100, "abc'def"))

# Query the database and obtain data as Python objects
>>> cur.execute("SELECT * FROM test;")
>>> cur.fetchone()
(1, 100, "abc'def")

>>> conn.commit()
>>> cur.close()
"""
