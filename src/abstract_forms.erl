-module(abstract_forms).
-author("Zephyr Pellerin <zv@nxvr.org>").
-import(erl_syntax, [atom_value/1,
                     attribute_name/1,
                     attribute_arguments/1,
                     type/1
                    ]).
-export([translate_module/2,
         get_module/1,
         get_exports/1,
         get_attribute/2
        ]).

-include("ex_translate.hrl").

translate_module(Forms, _Options) ->
  ExpressionTree = build_function_tree(Forms),
  build_initial_context(Forms, ExpressionTree).

%%% @doc
%%% Initializes the module context, returning a proplist with which we begin
%%% building up the abstract representation intended for consumption by Elixir.
%%% @end
-spec build_initial_context(forms(), #elixir_expr{}) -> #elixir_expr{}.
build_initial_context(Forms, ExpressionTree) ->
  #elixir_expr{
     qualifier = defmodule,
     metadata  = [ ?ElixirCtx ],
     arguments = [
                  % Module Name
                  { '__aliases__', [{alias, false}], [ get_module(Forms) ]},
                  % Begin the module definiton block
                  [{do, ExpressionTree}]
                 ]
    }.

%%% @doc
%%% Return a list of our function's named arguments.
%%% @end
-spec function_arguments(form()) -> #elixir_expr{}.
function_arguments(Func) ->
  {clause, _LineNum, Args, _B, _Body} = hd(erl_syntax:function_clause(Func)),
  [ #elixir_expr{qualifier=Arg, metadata=[], arguments='Elixir'} ||  {var, _, Arg} <- Args ].

-spec process_function(form()) -> #elixir_expr{}.
process_function(Func) ->
  {tree, atom, _, FuncName} = erl_syntax:function_name(Func),
  ArgList = function_arguments(Func),
  ExpressionTree = [ translate_expression(type(Expression), Expression) ||
                     Expression <- erl_syntax:function_clauses(Func) ],
  #elixir_expr{
     qualifier = def,
     metadata  = [ ?ElixirCtx ],
     arguments = [
                  { FuncName, [], ArgList },
                  [ { do, ExpressionTree } ]
                 ]
    }.

%%% @doc
%%% Process our forms, appending each function to the initial module context block
%%% @end
-spec build_function_tree([any()], forms()) -> #elixir_expr{}.
build_function_tree(Forms) ->
  build_function_tree([], Forms).
build_function_tree(InitialContext, [F|Forms]) ->
  case type(F) == function of
    true  -> process_function(F) ++ build_function_tree(InitialContext, Forms);
    false -> build_function_tree(InitialContext, Forms)
  end;
build_function_tree(_InitialContext, []) ->
  [].

%%% @doc
%%% Derive a list of this module's exports so we can declare our functions to be
%%% either private or public.
%%% @end
-spec get_exports(forms()) -> none | [any()].
get_exports(Forms) ->
  find_attribute(export, Forms).


%%% Retuns an attribute from the parse_transform Form List
-spec get_attribute(atom(), [any()]) -> undefined | atom().
get_attribute(A, Forms) ->
  case find_attribute(A, Forms) of
    false -> erl_syntax:atom(undefined);
    Other -> Other
  end.

%%% Search the form list for an attribute
-spec find_attribute(atom(), [any()]) -> none | [any()].
find_attribute(A, [F|Forms]) ->
  case type(F) == attribute andalso atom_value(attribute_name(F)) == A of
    true  -> attribute_arguments(F);
    false -> find_attribute(A, Forms)
  end;
find_attribute(_, []) ->
  false.

% Returns the name of the module being compiled.
-spec get_module([any()]) -> atom().
get_module(Forms) -> atom_value(hd(get_attribute(module, Forms))).


%%% @doc
%%% translate_expression recursively converts an erlang AST expression into a
%%% statement that we can concatenate into the Elixir syntax tree.
%%% @end

translate_expression(integer, Expression)  ->
  erl_syntax:integer_value(Expression);

translate_expression(variable, Expression) ->
  #elixir_expr{
     qualifier = erl_syntax:variable_name(Expression),
     metadata  = [],
     arguments = 'Elixir'
    };

translate_expression(infix_expr, Expression) ->
  #elixir_expr{
     qualifier = erl_syntax:infix_expr_operator(Expression),
     metadata  = ?ElixirEnv,
     arguments = [
                  translate_expression(
                    type(erl_syntax:infix_expr_left(Expression)), Expression
                   ),
                  translate_expression(
                    type(erl_syntax:infix_expr_right(Expression)), Expression
                   )
                 ]
    };

translate_expression(case_expr, Expression) ->
  #elixir_expr{
     qualifier = 'case',
     metadata  = ?ElixirEnv,
     arguments = [{do,
                   { '->', [],
                     % Take the erl_syntax:match_expr_body clauses as a generator, parsing each expression.
                     [ translate_expression(type(Clause), Clause) || Clause <- erl_syntax:case_expr_clauses(Expression) ]
                   }
                  }
                 ]
    }.
