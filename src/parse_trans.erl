%%% This module is a simple mechanism to print and return the parse tree
-module(parse_trans).
-import(erl_syntax, [atom_value/1,
                     attribute_name/1,
                     attribute_arguments/1,
                     function_clauses/1,
                     type/1
                    ]).
-include("ex_translate.hrl").
-export([parse_transform/2]).

% A convienence method for performing functions on the nodes in a postorder fashion.
postorder(F, Tree) ->
  F(case erl_syntax:subtrees(Tree) of
      [] -> Tree;
      List ->  [[postorder(F, Subtree) || Subtree <- Group] || Group <- List]
    end).

parse_transform(Forms, Options) ->
    io:fwrite("Options = ~p~n", [Options]),
    % io:fwrite("Forms = ~p~n", [Forms]),
    Module = abstract_forms:get_module(Forms),
    [ io:fwrite("~p~n~n~n~n~n", [Subtree]) || Subtree <- Forms ],
    io:fwrite("Module = ~p~n", [Module]),
    Module.
