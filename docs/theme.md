# Theme

Themes are written in Lua along with HTML, CSS and Javascript. The theme is
responsible for retrieving data and rendering views. The core takes care of
routing, connecting to the database, marshalling data etc.

## Theme contents

A theme should consist of 2 Lua files to function:

 - page.lua
 - page_404.lua

### page.lua

This file is ran when a user visists a page on the website that is found in the
database. To render the view this file can use the
[get_current_page](#get_current_page()) API function to retrieve the current
page. 

If the page is not found, the `page_404.lua` is ran instead.

### page_404.lua

This file is responsible for rendering a view when the requested page does not
exist.

## API reference

### get_current_page()

This function retrieves the page that the user is currently visiting. The core
handles the routing and selects the current page from the database.

A `table` that represents the current page is returned. 

### get_navigation()

This function retrieves a `list` of pages that represents the navigation of the
website.
