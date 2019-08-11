# Type directed autocompletion for Elm

[![Build Status](https://travis-ci.org/kirchner/elm-type-directed-autocomplete.svg?branch=master)](https://travis-ci.org/kirchner/elm-type-directed-autocomplete)

This is an **experimental** tool which generates autocompletion suggestions for
Elm code based on available type information.

There are basically two steps to this:

  1. Given a syntactically correct Elm file, it will try to infer the type of
     the expression at the current cursor position.
  2. If this was successfull, it will generate new expressions which are of
     this type using locally available values. (Take a look at
     [elm/projects](https://github.com/elm/projects#type-directed-autocomplete)
     for more information on this idea.)

Please keep in mind, that the type inference algorithm is not completely
implemented, yet. So you might run into weird bugs or may not get any
completions at all.


## Run

If you want to try it out locally, you can run

```bash
$ npm run elm
$ npm link
```

Then, inside the folder which contains your `elm.json` file, run

```bash
$ elm-autocomplete --server
```

To get suggestions at a certain position of some Elm file, you have to run the
following command in a different terminal (again inside the folder with your
`elm.json` file)

```bash
$ cat src/Main.elm | elm-autocomplete --file src/Main.elm --column 5 --row 10
```

If successfull, this should output a list of possible expressions.


### Vim integration

There is a very basic vim integration available using
[coc.nvim](https://github.com/neoclide/coc.nvim). To use this you have to copy
[`vim/elm.js`](https://github.com/kirchner/elm-type-directed-autocomplete/blob/master/vim/elm.js)
to `~/.vim/coc-extensions/elm.js`. Then, if you have the server running, you
should get the autocompletion suggestions within vim.



## Development

There are some tests available, which can be run via

```bash
$ npx elm-test --watch
```

If you are not satisfied with the suggestions, you can try to adjust the
completion generator yourself. The CLI tool uses the `default` Generator within
the `Generator` module. So, adjusting this and rebuilding the CLI might be
a good place to start!


## Credits

* The CLI-wrapper is an adaptation of
  [zwilias/elm-xref](https://github.com/zwilias/elm-xref)
* The parsing of the Elm sources is done using
  [stil4m/elm-syntax](https://github.com/stil4m/elm-syntax)
* When implementing the type inference algorithm, the article [Write You
  a Haskell](http://dev.stephendiehl.com/fun/index.html) was very helpfull, as
  well as [the actual Elm compiler](https://github.com/elm/compiler) itself.
