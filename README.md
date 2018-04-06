# Elm Glob

A module for glob/fn-match based string matching. Patterns are parsed and converted to regexes and
matched using the core [Regex](http://package.elm-lang.org/packages/elm-lang/core/latest/Regex) library.


## Features

### Syntax

- **&ast;** - matches everything
- **?** - matches any single character
- **[set]** - matches any character in the set
- **[!set]** - matches any character not in the set

The set syntax also supports ranges. eg. `a-z`, `0-6`, etc. Only ASCII letters & numbers as I'm
unclear on how to properly support other locales.


### Options

Available with the `defaultOptions` record which has the `Options` type.

- **enableAsterisk** (default: True)
  Can be used to disable the handling of '\*'

- **enableQuestionMark** (default: True)
  Can be used to disable the handling of '?'

- **enableBrackets** (default: True)
  Can be used to disable the handling of characters sets that use '[]'


Available as an independent function:

- **Glob.caseInsenstive**
  Sets 'caseInsenstive' on the underlying Regex.


## Contribute

Run:

- `yarn` or `npm install` to set up
- `yarn test` to run the tests

Please add tests to help cover any changes you are making.

The name & API are subject to change. I would welcome any help & advice on making them better.
