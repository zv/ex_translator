# ex_translator
ex_translator is a tool to translate Erlang to Elixir source code

### Functionality

This tool translates elixir to erlang source code by walking the AST of the
original erlang source code, provided by a hook specified in the erlang
`compile` module facility. Serializing, then scanning, parsing  and converting
that into another tuple-set AST that we can deserialize from within Elixir
through the Macro facilities.
