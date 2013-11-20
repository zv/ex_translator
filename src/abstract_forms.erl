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
  Module = get_module(Forms),
  io:fwrite("Translating Module: ~p~n", [Module]),
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
  {clause, _LineNum, Args, _B, _Body} = hd(erl_syntax:function_clauses(Func)),
  [ #elixir_expr{qualifier=Arg, metadata=[], arguments='Elixir'} ||  {var, _, Arg} <- Args ].

-spec process_function(form()) -> #elixir_expr{}.
process_function(Func) ->
  {tree, atom, _, FuncName} = erl_syntax:function_name(Func),
  ArgList = function_arguments(Func),
  io:fwrite("Processing function: ~p(~p)~n", [FuncName, ArgList]),
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
  Func = case type(F) == function of
           true  -> process_function(F);
           false -> []
         end,
  [Func] ++ build_function_tree(InitialContext, Forms);
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
%%% A helper comprehension translating each constituent expression
%%% @end
translate_elements(Elements) ->
  [ translate_expression(type(E), E) || E <- Elements ].

%%% @doc
%%% translate_expression recursively converts an erlang AST expression into a
%%% statement that we can concatenate into the Elixir syntax tree.
%%% @end

%%
%% Literal Expressions
%%

translate_expression(integer, Expression)  ->
  erl_syntax:integer_value(Expression);

translate_expression(float, Expression)  ->
  erl_syntax:float_value(Expression);

translate_expression(atom, Expression) ->
  erl_syntax:atom_value(Expression);

translate_expression(list, Expression) ->
  translate_elements(erl_syntax:list_elements(Expression));

% Tuples of size 2 are represented as a literal in Elixir
translate_expression(tuple, Expression) when 2 == tuple_size(Expression) ->
  translate_elements(erl_syntax:tuple_elements(Expression));

%%
%% Composite Expressions
%%

% Tuples *not* containing exactly 2 elements are represented by a call to :{}
translate_expression(tuple, Expression) ->
  #elixir_expr{
     qualifier = '{}', metadata  = [],
     arguments = translate_elements( erl_syntax:tuple_elements(Expression) )
    };

translate_expression(case_expr, Expression) ->
  #elixir_expr{
     qualifier = 'case', metadata  = ?ElixirEnv,
     arguments = [
                  {do,
                   { '->', [],
                     % Take the erl_syntax:match_expr_body clauses as a
                     % generator, parsing each expression.
                     translate_elements( erl_syntax:case_expr_clauses(Expression) )
                   }
                  }
                 ]
    };

% Clause is a generic expression representing a new named or anonymous function
% body, block expression, or any case where a new scope will be created in a
% block.
translate_expression(clause, Expression) ->
  #elixir_expr{
     qualifier = do, metadata  = '__block__',
     arguments = translate_elements( erl_syntax:clause_body(Expression) )
    };

% Generators represent expressions of the form X <- [1,2,3]
translate_expression(generator, Expression) ->
  % Pattern represents the variable being bound
  P = erl_syntax:generator_pattern(Expression),
  % Body contains the expression or list literal
  B = erl_syntax:generator_body(Expression),
  #elixir_expr{
     qualifier = generator,
     metadata  = [],
     arguments = [
                  translate_expression(type(P), P),
                  translate_expression(type(B), B)
                 ]
    };

% Infix expressions are simple two parameter operators, such as arithmetic.
translate_expression(infix_expr, Expression) ->
  #elixir_expr{
     qualifier = erl_syntax:infix_expr_operator(Expression),
     metadata  = ?ElixirEnv,
     arguments = [
                  translate_expression(
                    type( erl_syntax:infix_expr_left(Expression) ), Expression
                   ),
                  translate_expression(
                    type( erl_syntax:infix_expr_right(Expression) ), Expression
                   )
                 ]
    };

translate_expression(list_comp, Expression) ->
  Comprehension      = erl_syntax:list_comp_template(Expression),
  #elixir_expr{
     qualifier = lc,
     metadata  = [],
     arguments = [  translate_expression( type(Comprehension), Comprehension ) ]
                 ++ translate_elements( erl_syntax:list_comp_body(Expression) )
    };

translate_expression(variable, Expression) ->
  #elixir_expr{
     qualifier = erl_syntax:variable_name(Expression),
     metadata  = [],
     arguments = 'Elixir'
    }.
