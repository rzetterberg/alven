# Testing

The project is tested in 3 different groups: unit, integration and acceptance.

## Unit

These tests prove that single functions produce the expected output. Typically
algorithms and utility functions that are pure. They use QuickCheck and HUnit.

They are written in Haskell and is integrated in the project.

## Integration

These tests prove that different subsystems of the project works together as
expected. Typically database model operations, Lua-Haskell integration, etc.

They are written in Haskell and is integrated in the project.

## Acceptance

These tests prove that the project works as expected from a users perspective.

The user perspective is considered the a minimal browser that does not support
javascript, css, flash etc. The project is built with progressive enhancement
in mind to be able to serve all types of users.

They written separately in python using behave, cucumber and mechanize.
