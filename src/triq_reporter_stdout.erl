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
