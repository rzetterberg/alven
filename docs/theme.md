# Theme

Themes are written in Lua along with HTML, CSS and Javascript. The theme is
responsible for retrieving data and rendering views. The core takes care of
routing, connecting to the database, marshalling data etc.

## Theme contents

A theme should consist of 1 Lua file to function:

 - page.lua

### page.lua

This file is ran when a user visists a page on the website that is found in the
database. To render the view this file can use the
[get_current_page](#get_current_page()) API function to retrieve the current
page. 

## API reference

### table get_current_page(void)

This function retrieves the page that the user is currently visiting. The core
handles the routing and selects the current page from the database.

A `table` that represents the current page is returned, or `nil` if no page was
found.

### list get_pages(void)

This function retrieves a `list` of all pages in the database.

### string read_theme_file(string)

Reads a file using the relative path where the root is the theme directory.
Returns a `string` with the file contents on success and throws an error on
failure.
