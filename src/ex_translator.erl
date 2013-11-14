#!/usr/bin/env escript
% -module(ex_translator).
-author('zv@nxvr.org').

% -behaviour(application).

%-export([start/2, stop/1]).

main([String]) ->
  % Read our file args
  io:format(String ++ "~n"),

  Filename = String,

  file:copy(Filename, "pretranslate.erl"),

  % Compile our file w/ the parse_transform defined in parse_trans, write out
  % various debugging data. Do not produce object file.
  compile:file(pretranslate, [
                              verbose, % be verbose
                              {parse_transform, parse_trans} % Run parse_transform/2 in the code from the module parse_trans
                              ]).
