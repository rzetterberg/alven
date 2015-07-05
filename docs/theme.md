# Theme

Themes are written in Lua along with HTML, CSS and Javascript. The theme is
responsible for retrieving data and rendering views. The core takes care of
routing, connecting to the database, marshalling data etc.

## Theme contents

A theme should consist of 1 Lua file to function:

 - main.lua

### main.lua

This file is ran when a user visists a page on the website.

## API reference

All API functions can be accessed via the `kael` module. If you want to
call the function `output` with input `hello` you do this:
`kael.output("hello")`.

### void output(string)

Outputs the given string to the buffer that is sent to the browser.
Can be used multiple times.

### string get_theme_url(string)

Returns an absolute path to the given theme file. The input argument expects
a path relative to the theme root.

For example if the website domain is `example.com` and the file `logotype.png`
inside the `img` directory should be loaded, and you call
`get_theme_url("img/logotype.png)` you will recieve
`http://example.com/static/theme/img/logotype.png`.

### table get_current_page(void)

This function retrieves the page that the user is currently visiting. The core
handles the routing and selects the current page from the database.

A `table` that represents the current page is returned, or `nil` if no page was
found.

### table get_pages(void)

This function retrieves a `table` (list) of all pages in the database.

### string read_theme_file(string)

Reads a file using the relative path where the root is the theme directory.
Returns a `string` with the file contents on success and throws an error on
failure.
