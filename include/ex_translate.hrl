-define(ElixirCtx, {context, 'Elixir'}).
-define(ElixirEnv, [?ElixirCtx, {import, 'Kernel'}]).

-type form()    :: any().
-type forms()   :: [form()].

-record(elixir_expr, {
  % describes the expression type or qualifier
  qualifier :: tuple() | atom,
  % list of metadata, it may hold information like the node line number;
  metadata  :: list(),
  % list of arguments for the function call or when an atom it means the tuple represents a variable
  arguments :: list() | atom
}).
