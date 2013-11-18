%% This module is a simple mechanism to print and return the parse tree
-module(parse_trans).
-import(erl_syntax, [atom_value/1,
                     attribute_name/1,
                     attribute_arguments/1,
                     type/1
                    ]).
-export([parse_transform/2]).

-type form()    :: any().
-type forms()   :: [form()].
-type elixir_form() :: any().
-type elixir_forms() :: [elixir_form()].

%% @doc
%% Initializes the module context, returning a proplist with which we begin
%% building up the abstract representation intended for consumption by Elixir.
%% @end
-spec build_initial_context(forms()) -> elixir_forms().
build_initial_context(Forms) ->
    Module = get_module(Forms),
    % Return initial AST module, does not include initial `do` block
    {defmodule,
     [{context, list_to_atom("Elixir")}],
     [{list_to_atom("__aliases__"),
       [{alias, false}], [Module]},
       [{do, nil}] % Begin the module definiton block
     ]}.

parse_expression(variable, Expression) ->
  {var, _LN, VarName} = Expression,
  {VarName, [], list_to_binary("Elixir")}.

parse_expression(integer, Expression) ->
  {integer, _LN, Int} = Expression,
  Int.

parse_expression(infix_expr, Expression) ->
  ExprOperator = erl_syntax:infix_expr_operator(Expression),
  {
   ExprOperator, [{context, 'Elixir'}, {import, 'Kernel'}],
   [
    % Concatenate the right and left infix operators as Erlang will always break
    % out the syntax tree into distinct operations of the appropriate arity
    parse_expression(type(erl_syntax:infix_expr_left(Expression)), Expression),
    parse_expression(type(erl_syntax:infix_expr_right(Expression)), Expression)
   ]
  }.
get_exports(Forms) ->
  { attribute, _, export, Exports } = find_attribute(export, Forms),
  Exports.


%% Retuns an attribute from the parse_transform Form List
-spec get_attribute(atom(), [any()]) -> undefined | atom().
get_attribute(A, Forms) ->
    case find_attribute(A, Forms) of
        false ->
            erl_syntax:atom(undefined);
        Other ->
            Other
    end.

%% Search the form list for an attribute
-spec find_attribute(atom(), [any()]) -> none | [any()].
find_attribute(A, [F|Forms]) ->
    case type(F) == attribute
        andalso atom_value(attribute_name(F)) == A of
        true ->
            attribute_arguments(F);
        false ->
            find_attribute(A, Forms)
    end;
find_attribute(_, []) ->
    false.

% Returns the name of the module being compiled.
-spec get_module([any()]) -> atom().
get_module(Forms) -> atom_value(hd(get_attribute(module, Forms))).

% A convienence method for applying a function to each subtree.
postorder(F, Tree) ->
  F(case erl_syntax:subtrees(Tree) of
      [] -> Tree;
      List -> [ postorder(F, Subtree) || Subtree <- List ]
    end).

parse_transform(Forms, Options) ->
    io:fwrite("Options = ~p~n", [Options]),
    io:fwrite("Forms = ~p~n", [Forms]),
    Module = get_module(Forms),
    io:fwrite("Module = ~p~n", [Module]),
    Module.
