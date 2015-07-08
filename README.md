# Alven

[![Build Status](https://travis-ci.org/rzetterberg/alven.svg?branch=master)](https://travis-ci.org/rzetterberg/alven)

**Version:** 0.1.0 (not suitable for production)

`Alven` is a CMS written in [Haskell](https://www.haskell.org) that embeds
[Lua](http://www.lua.org) for writing themes.

`Alven` is based on the [Yesod Web Framework](http://www.yesodweb.com/).

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
