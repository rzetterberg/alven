# Alven

`Alven` is a CMS which core functionality is written in
[Haskell](https://www.haskell.org) and allows users to write theme layout and
functionality in [Lua](http://www.lua.org).

This project aims to use the best of both worlds to provide a CMS which is
fast, stable and easy to write themes for.

**Version:** 0.1.0 (not suitable for production)

## Theme basics

All communication between the core system and the Lua theme is provided by
the Lua module `alven`. Here's a simple theme that uses a `mustache` template
to display the content of the current page:

**main.lua**
```lua
-- The lustache module is simply placed in the same directory
local lustache = require "lustache"

-- Files within the theme directory can be read by relative path
template = alven.read_theme_file("page_view.mustache")

result = lustache:render(
   template,
   { curr_page = alven.get_current_page() }
)

-- Each time output is called the given string is appended to a buffer
-- that later becomes the body of the response.
alven.output(result)
```

**page_view.mustache**
```html
<!doctype html>
<html class="no-js" lang="en">
  <head>
    <meta charset="UTF-8">
    <title>{{curr_page.name}}</title>
    <meta name="viewport" content="width=device-width,initial-scale=1">
    <link rel="stylesheet" href="http://getbootstrap.com/dist/css/bootstrap.min.css" />
  </head>
  <body>
    <div class="container">
      <div class="row">
        <div class="col-lg-12">
          {{{curr_page.body}}}
        </div>
      </div>
    </div>
  </body>
</html>
```

## Admin interface basics

The admin interface is part of the core functionality written in Haskell. The
interface has a responsive design and follows the progressive enhancement
strategy. Accessability and overall user experience is very important for this
project.

Currently the admin interface has the following features:

### Page management

As a user you can create, edit and remove pages.

Each page has a name/title, a slug/permalink and a body. The body is written in
[Markdown](http://daringfireball.net/projects/markdown/) and the page form has
preview functionality.

Each page can be marked as public/private to allow work in progress pages that
only are shown to logged in users.

### User management

As a user you can create, edit and remove users.

Users are identified by email and can either be a normal user or a admin user.
When a user is created a email is sent to the user with a link to a registration
page where the user can enter the password to be used with the account.
