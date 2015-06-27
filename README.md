# Kael

`Kael` is a CMS which core functionality is written in
[Haskell](https://www.haskell.org) and exposes a [Lua](http://www.lua.org) API
that allows users to write themes in Lua, HTML, CSS and Javascript.

The main goals of the project are:

- Is fun to use
- Is stable
- Built using modern technology

## Features

The project functionality is described by Cucumber feature files in the folder
`features`. The features should be focused on describing user functionality, and
avoid describing internal functionality.

The acceptance tests that use those features shoudle be simple and show that a
minimal client (no javascript, no css) can use those features. The project is
built with progressive enhancement in mind to be able to serve all types of
users.
