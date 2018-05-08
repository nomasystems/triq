%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% Copyright 2017-2018 Triq authors
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(triq_reporter_stdout).
-export([report/2, report/3]).

report(testing, [Module, Fun]) ->
    io:format("Testing ~p:~p/0~n", [Module, Fun]);
report(pass,_) ->
    io:format(".");
report(skip,_) ->
    io:format("x");
report(fail,false) ->
    io:format("Failed!~n");
report(fail,Value) ->
    io:format("Failed with: ~p~n", [Value]);
report(check_failed, [Count, Error]) ->
    io:format("~nFailed after ~p tests with ~p~n", [Count,Error]);
report(counterexample, CounterExample) ->
    io:format("Simplified:~n"),
    print_counter_example(CounterExample);
report(success, Count) ->
    io:format("~nRan ~p tests~n", [Count]).

report(_Subject, _Data, true) -> ok;
report(Subject, Data, false) -> report(Subject, Data).

print_counter_example(CounterExample) ->
    lists:foreach(fun({Syntax,_Fun,Val,_Dom}) ->
                          io:format("\t~s = ~w~n", [Syntax,Val])
                  end,
                  CounterExample).
