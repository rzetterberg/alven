import mechanize, os, re, cookielib, os.path
from bs4 import BeautifulSoup

def before_all(context):  
    print(">> Before all\n")
    
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
