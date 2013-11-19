%%% This module is a simple mechanism to print and return the parse tree
-module(parse_trans).
-author("Zephyr Pellerin <zv@nxvr.org>").
-include("ex_translate.hrl").
-export([parse_transform/2]).

parse_transform(Forms, Options) ->
    %io:fwrite("Options = ~p~n", [Options]),
    %io:fwrite("Forms = ~p~n", [Forms]),
    Module = abstract_forms:get_module(Forms),
    TranslatedModule = try
      abstract_forms:translate_module(Forms, Options)
    catch
      _:_ -> io:format("Backtrace ~p~n", [erlang:get_stacktrace()])
    end,
    io:fwrite("TranslatedModule = ~p~n", [TranslatedModule]),
    io:fwrite("Module = ~p~n", [Module]),
    Module.
