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
-type options() :: [{atom(), any()}].

%% @doc
%% Initializes the module context and returns a tuple to begin building up the
%% abstract representation intended for consumption by Elixir.
%% @end
-spec build_initial_context(forms(), options()) -> tuple().
build_initial_context(Forms, Options) ->
    Module = get_module(Forms),
    % Return initial AST module, does not include initial `do` block
    {defmodule,
     [{context, binary_to_atom(<<"Elixir">>, utf8)}],
     [{binary_to_atom(<<"__aliases__">>, utf8),
       [{alias, false}],
       [binary_to_atom(Module, utf8)]},
       [{do, nil}]
     ]}.

%% @doc
%% Derive a list of this module's exports so we can declare our functions to be
%% either private or public.
%% @end
-spec get_exports(forms()) -> list().
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

parse_transform(Forms, Options) ->
    io:fwrite("Options = ~p~n", [Options]),
    io:fwrite("Forms = ~p~n", [Forms]),
    Module = get_module(Forms),
    io:fwrite("Module = ~p~n", [Module]),
    Module.
