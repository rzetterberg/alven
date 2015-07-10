### API functions

All API functions can be accessed via the `alven` module. If you want to
call the function `output` with input `hello` you do this:
`alven.output("hello")`.

**<a name="output"></a> void output(string)**

Outputs the given string to the buffer that is sent to the browser.
Can be used multiple times.

**<a name="get_theme_url"></a> string get\_theme\_url(string)**

Returns an absolute path to the given theme file. The input argument expects
a path relative to the theme root.

For example if the website domain is `example.com` and the file `logotype.png`
inside the `img` directory should be loaded, and you call
`get_theme_url("img/logotype.png)` you will recieve
`http://example.com/static/theme/img/logotype.png`.

**<a name="get_current_page"></a> page get\_current\_page(void)**

This function retrieves the page that the user is currently visiting. The core
handles the routing and selects the current page from the database.

A `table` that represents the current page is returned, or `nil` if no page was
found.

**<a name="get_pages"></a> [Page] get\_pages(void)**

This function retrieves a `table` (list) of all pages in the database.

**<a name="read_theme_file"></a> string read\_theme\_file(string)**

Reads a file using the relative path where the root is the theme directory.
Returns a `string` with the file contents on success and throws an error on
failure.

### API types

**Page**

Page is a table that represents a normal text page created in the admin panel.

It consists of the following fields:

 - string name
 - string permalink
 - string body
 - boolean is\_public
 - string url
