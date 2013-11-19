# ex_translator
ex_translator is a tool to translate Erlang to Elixir source code

## Functionality

This tool translates elixir to erlang source code by walking the AST of the
original erlang source code, provided by a hook specified in the erlang
`compile` module facility. Serializing, then scanning, parsing  and converting
that into another tuple-set AST that we can deserialize from within Elixir
through the Macro facilities.

## Getting Started
To start working with ex_translator, simply run the following commands

```
  ./rebar get-deps
  ./rebar compile
  ./rebar escriptize
```

Then you can run `./ex_translate $FILE_NAME`
