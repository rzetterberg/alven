# Alven

[![Build Status](https://travis-ci.org/rzetterberg/alven.svg?branch=master)](https://travis-ci.org/rzetterberg/alven)

**Version:** 0.1.0 (not suitable for production)

`Alven` is a CMS written in [Haskell](https://www.haskell.org) that embeds
[Lua](http://www.lua.org) for writing themes.

The project comes with an admin interface where the user can manage pages,
accounts and navigation menus. 

The Lua theme takes care of retrieving the content created in the admin
interface and displaying it to the visitor of the website.

This project tries to solve a specific problem: You want to use `Yesod` for your
project, but you need to be able to change the way website layout and functionality
without having to setup a development environment and know `Haskell/Yesod`.

By using `Haskell` and `Yesod` for the core functionality and the admin
interface the project can achieve high performance and stability. If you compile
the project statically you also get an application that is easy to deploy.

By using `Lua` for themes it becomes really easy to write themes and the project
does not need to be recompiled after changes. After the project has been
deployed all you need to work on the theme is a text editor and a FTP client
(or other means to upload changes to the server).

## Quick technical overview

`Alven` is written to be compiled with `GHC 7.8.*` using the
[Stackage LTS](https://www.stackage.org/) version set.

It is based on the [Yesod Web Framework](http://www.yesodweb.com/).

It is written to be deployed on `GNU/Linux`.

Development and testing is performed inside docker containers to allow easy
and consistent setup. All images used in the project are based on the offical
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

## Try out the project

To try out the project you first need to build the containers:

```bash
./build_docker.sh
```

(If you want to verify the docker setup, you can view the
dockerfiles in the `config/docker` directory for each image.)

Then you start the builder and database containers:

```bash
./start_builder.sh
```

This will start the database container in the background, start the builder
in the foreground and drop you into a shell inside it. Also, the project
folder will be mount as volume in the container. Which means you can make
changes to your project outside the container while it's running.

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
