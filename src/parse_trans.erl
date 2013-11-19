%%% This module is a simple mechanism to print and return the parse tree
-module(parse_trans).
-author("Zephyr Pellerin <zv@nxvr.org>").
-include("ex_translate.hrl").
-export([parse_transform/2]).

parse_transform(Forms, Options) ->
    %io:fwrite("Options = ~p~n", [Options]),
    %io:fwrite("Forms = ~p~n", [Forms]),
    Module = abstract_forms:get_module(Forms),
    TranslatedModule = abstract_forms:translate_module(Forms, Options),
    io:fwrite("TranslatedModule = ~p~n", [TranslatedModule]),
    io:fwrite("Module = ~p~n", [Module]),
    Module.
