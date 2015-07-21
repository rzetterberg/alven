![Alven logotype](https://raw.githubusercontent.com/rzetterberg/alven/master/assets/alven-symbol-text.png)

[![Build Status](https://travis-ci.org/rzetterberg/alven.svg?branch=master)](https://travis-ci.org/rzetterberg/alven)

**Version:** 0.3.0 (not suitable for production)

**License:** MIT (see LICENSE file)

**Main goals:**
- Easy to use
- High performant
- Stable

--------------------------------------------------------------------------------

`Alven` is a CMS written in [Haskell](https://www.haskell.org) that embeds
[Lua](http://www.lua.org) for writing themes.

The project comes with an admin interface where the user can manage pages,
accounts and navigation menus. Pages are written in
[Markdown](http://daringfireball.net/projects/markdown/) in the admin
interface.

Showing the page content to the visitor is then handled by the Lua theme.

The project exports a Lua module called `alven`, which can be used in the theme
to get resources from the CMS (such as page lists, page content,
internal URLs etc.)

## Want to know more?

If you think this sounds interesting, you can
[watch a screen recording](https://www.youtube.com/watch?v=2UEViLWUCRg)
of me trying out the project using the development build.

Or if you want to get your hands dirty, there are two options: use the
development build or the pre-compiled binary.

The development build takes longer to setup, but you can use it on any GNU/Linux
distribution since everything is setup inside Docker containers.

The pre-compiled binary is quick to setup, but is currently only compiled for
Debian Jessie x86_64 hosts. 

See chapters further down for instructions.

## Quick technical overview

`Alven` is written to be compiled with `GHC 7.8.*` using the
[Stackage LTS](https://www.stackage.org/) version set.

It is based on the [Yesod Web Framework](http://www.yesodweb.com/).

It is written to be deployed on `GNU/Linux`.

Development and testing is performed inside [Docker](https://www.docker.com/)
containers to allow easy and consistent setup. All images used in the project
are based on the offical
[`haskell:7.8`](https://registry.hub.docker.com/_/haskell/) and 
[`debian:jessie`](https://registry.hub.docker.com/_/debian/) images.

There are 3 docker containers:

- The builder which is used to build the whole project
- The database which runs the postgres database for testing and development
- The tester which is used to run the external acceptance tests

Testing is performed in 3 levels: unit, integration and acceptance.

Unit and integration tests are written in `Haskell` and uses `hspec` for
structure.

Acceptance tests are written in `Python` and uses `cucumber` and `mechanize` to
test the admin interface from a minimal browser (no javascript, css, etc.)
point of view.

The embedded [Lua intepreter](http://www.lua.org/manual/5.1/) is version `5.1.4`.

## Try out the project (using development build)

### 1. Build the Docker containers

To try out the project you first need to build the containers:

```bash
./build_docker.sh
```

(If you want to verify the docker setup, you can view the
dockerfiles in the `config/docker` directory for each image.)

Note that building the containers can take quite a while since all cabal
dependencies are installed during that process.

### 2. Start the builder and database container

Then you start the builder and database containers:

```bash
./start_builder.sh
```

This will start the database container in the background, start the builder
in the foreground and drop you into a shell inside it. Also, the project
folder will be mount as volume in the container. Which means you can make
changes to your project outside the container while it's running.

### 3. Start the development server

After that you simply start the development server:

```bash
cd src
yesod devel
```

(If you get an error saying the database is starting up, just re-run
`yesod devel`)

After the development server has started you can access the project in your
webbrowser at [http://localhost:3000](http://localhost:3000).

The development server is setup to use the builder containers' IP for URLs. So
the first link you click in the interface will redirect you to using the IP
instead of localhost.

To access the admin interface you goto `/admin`. Currently the project is setup
to allow user registration for anyone. When a registration is submitted the
verification link is printed to the console. Copy the link from the console and
open it in your browser to finish the registration.

### 4. Add a Lua theme

By default the project does not come with a Lua theme, but you can use one of
the example themes used in the integration tests.

`Alven` tries to open the file `main.lua` in the folder `static/theme`. So, to
use the first example just run:

```bash
cp test/static/lua/examples/page_list static/theme -R
```

Now you can create pages in the admin interface and view them to see how the
theme renders the content.

You can make changes to the theme and just refresh the page in your browser
to see the changes.

You could also use the example theme below!

## Try out the project (using pre-compiled binary)

Here's how to deploy the current version on a `Debian Jessie x86_64` host. 

Download the package and unpack it:

```bash
wget https://github.com/rzetterberg/alven/releases/download/v0.3.0/alven-0.3.0-linux-x86_64.tar.gz
tar xzf alven-0.3.0-linux-x86_64.tar.gz
```

This will create a directory called `alven` which contains the binary executable
and the application data files.

Install the dependencies needed to run the project:

```bash
sudo apt-get install -y postgresql-common postgresql-9.4
```

Then you need to setup the database and user. Let's say the database name and
user will be `alven` and the password will be `testpass`.

First you create the PostgreSQL user and database:

```bash
sudo -u postgres psql --command \
    "CREATE USER alven WITH SUPERUSER PASSWORD 'testpass';" &&\
    createdb -O alven alven
```

Then you update the projects main settings file `alven/config/settings.yml` with the
right database credentials.

Then you are ready to start the executable:

```bash
cd alven
./alven config/settings.yml
```

By default `alven` will listen to port `3000`, but you can change that in the
settings file. 

## Example theme

Here's an example of a theme that uses [mustache](https://mustache.github.io/) to
render a template to display the content of the current page: 

```lua
-- To add a 3rd party Lua library you just put the source files in the theme
-- directory and use require
local lustache = require "lustache"

data = {
    -- Retrieves a list of all the pages that exists in the CMS
    page = alven.get_current_page();
    -- Retrives the current page the user is visiting. The CMS handles the
    -- routing and database calls to retrieve the content.
    pages = alven.get_pages(),
    -- A mustache lambda that can be used to render absolute URLs to files in
    -- the theme, such as CSS-files and images.
    theme_url = function (render, text)
       return alven.get_theme_url(text)
    end
}

-- Reads the whole mustache file and returns it as a string
template = alven.read_theme_file("base.mustache")
output = lustache:render(template, data)

-- Appends the given string to the output buffer that becomes the HTTP response
-- body after the Lua theme has finished execution.
-- This function can be called multiple times to build up the response
-- sequencially.
alven.output(output)
```

And here's what the `mustache` template looks like:

```html
<!doctype html>
<html class="no-js" lang="en">
  <head>
    <meta charset="UTF-8">
    <title>{{ page.name }}</title>
    <meta name="description" content="">
    <meta name="author" content="">
    <meta name="viewport" content="width=device-width,initial-scale=1">
  </head>
  <body>
    <img src="{{#theme_url}}img/logotype.png{{/theme_url}}" alt="Haskell logotype" />
    <ul>
      {{#pages}}
        <li>
          <a href="{{{url}}}">
            {{name}}
          </a>
        </li>
      {{/pages}}
    </ul>

    {{{ page.body }}}
  </body>
</html>
```

The theme directory has the following structure:

```bash
$ cd src/static/theme
$ tree
.
├── base.mustache
├── img
│   └── logotype.png
├── lustache
│   ├── context.lua
│   ├── renderer.lua
│   └── scanner.lua
├── lustache.lua
└── main.lua

2 directories, 7 files
```

With one page created in the CMS named "Index" and with the content:

```markdown
# Index 

This is the index page, and it does not contain anything interesting yet.
```

After the theme has ben run the result will be:

```html
<!doctype html>
<html class="no-js" lang="en">
  <head>
    <meta charset="UTF-8">
    <title>Index</title>
    <meta name="description" content="">
    <meta name="author" content="">
    <meta name="viewport" content="width=device-width,initial-scale=1">
  </head>
  <body>
    <img src="http://ip-of-the-container:3000/static/theme/img/logotype.png" alt="Haskell logotype" />
    <ul>
      
        <li>
          <a href="http://ip-of-the-container:3000/page/view/index">
            Index
          </a>
        </li>
      
    </ul>

    <h1>Index</h1><p>This is the index page, and it does not contain anything interesting yet.</p>
  </body>
</html>
```
