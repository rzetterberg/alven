# Kael

`Kael` is a CMS which core functionality is written in
[Haskell](https://www.haskell.org) and exposes a [Lua](http://www.lua.org) API
that allows users to write themes in Lua, HTML, CSS and Javascript.

The main goals of the project are:

- Is fun to use
- Is stable
- Built using modern technology

## Testing

The project is tested in 3 different levels: unit, integration and acceptance. 

### Unit

These tests prove that pure functions produce the expected output. They should
be written using QuickCheck.

### Integration

These tests prove that different subsystems of the project works together as
expected. For example check that database functions produce the expected output
from a known state.

### Acceptance

These tests prove that the project works as expected from a users perspective.

The user perspective is considered the web interface by using a minimal browser
that does not support javascript, css, flash etc. The project is built with
progressive enhancement in mind to be able to serve all types of users.
