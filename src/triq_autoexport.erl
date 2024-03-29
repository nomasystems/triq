%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% Copyright 2013-2018 Triq authors
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
%%
%% @author Richard Carlsson <carlsson.richard@gnail.com>
%% @author Kresten Krab Thorup <krab@trifork.com>
%% @copyright 2006-2014 Richard Carlsson
%% @private
%% @see triq
%% @doc Parse transform for automatic exporting of prop_ functions.

-module(triq_autoexport).

-define(DEFAULT_PROP_PREFIX, "prop_").

-export([parse_transform/2]).

-define(CHECK,check).

parse_transform(Forms, Options) ->
    %% io:format("FORMS ~n~p~n", [Forms]),
    PropPrefix = proplists:get_value(triq_prop_prefix, Options,
                                     ?DEFAULT_PROP_PREFIX),
    F = fun (Form, Set) ->
                t_form(Form, Set, PropPrefix)
        end,
    PropExports = sets:to_list(lists:foldl(F, sets:new(), Forms)),
    EUnit = maybe_gen_eunit(PropPrefix, Forms),
    EUnitExports = lists:map(fun ({function, _, Name, 0, _}) -> {Name, 0} end, EUnit),
    Forms1 = t_rewrite(Forms, PropExports ++ EUnitExports),
    add_eunit(Forms1, EUnit).

t_form({function, _L, Name, 0, _Cs}, S, PropPrefix) ->
    N = atom_to_list(Name),
    case lists:prefix(PropPrefix, N) of
        true ->
            sets:add_element({Name, 0}, S);
        false ->
            S
    end;
t_form(_, S, _) ->
    S.

gen_eunit(Forms, PropPrefix, Opts) ->
    EUnitGen = fun(Form, Set) ->
                       t_eunit_form(Form, Set, PropPrefix, Opts)
               end,
    sets:to_list(lists:foldl(EUnitGen, sets:new(), Forms)).

lists_last([]) -> [];
lists_last(L) -> lists:last(L).

maybe_gen_eunit(PropPrefix, Forms) ->
    TriqAttrs = lists:foldl(
                  fun({attribute,_,triq,Val}, Acc) ->
                          [Val|Acc];
                     (_, Acc) ->
                          Acc
                  end,
                  [], Forms),
    Attrs = lists_last(proplists:lookup_all(eunit, TriqAttrs)),
    case Attrs of
        {eunit, true} ->
            gen_eunit(Forms, PropPrefix, []);
        {eunit, Opts} ->
            gen_eunit(Forms, PropPrefix, Opts);
        _ ->
            []
    end.

check_args(Name, Line, Opts) ->
    case lists_last(proplists:lookup_all(runs, Opts)) of
        [] ->
            F = [{call,Line,{atom,Line,list_to_atom(Name)},[]}],
            {F, "triq : check ( "++Name++" ( ) )"};
        {runs, Runs} ->
            F = [{call,Line,{atom,Line,list_to_atom(Name)},[]},
                 {integer,Line,Runs}],
            {F, "triq : check ( "++Name++" ( ), "++integer_to_list(Runs)++" )"}
    end.

assertion(Name, Line, Opts) ->
    "prop_" ++ PropName = Name,
    TestName = list_to_atom(PropName ++ "_test_"),
    {CheckArgs, CheckCallStr} = check_args(Name, Line, Opts),
    {LineNum, Col} = Line,

    {function,Line,TestName,0,
     [{clause,Line,[],[],
       [{tuple,Line,
         [{atom,Line,timeout},
          {integer,Line,3600},
          {tuple,Line,
           [{integer,Line,LineNum},
            {'fun',Line,
             {clauses,
              [{clause,Line,[],[],
                [{block,Line,
                  [{call,Line,
                    {'fun',Line,
                     {clauses,
                      [{clause,Line,[],[],
                        [{'case',Line,
                          {call,Line,
                           {remote,Line,{atom,Line,triq},{atom,Line,check}},
                           CheckArgs},
                          [{clause,Line,[{atom,Line,true}],[],[{atom,Line,ok}]},
                           {clause,Line,
                            [{var,Line,'__V'}],
                            [],
                            [{call,Line,
                              {remote,Line,{atom,Line,erlang},{atom,Line,error}},
                              [{tuple,Line,
                                [{atom,Line,assertion_failed},
                                 {cons,Line,
                                  {tuple,Line,
                                   [{atom,Line,module},{atom,Line,mutations}]},
                                  {cons,Line,
                                   {tuple,Line,
                                    [{atom,Line,line},{integer,Line,LineNum}]},
                                   {cons,Line,
                                    {tuple,Line,
                                     [{atom,Line,col},{integer,Line,Col}]},
                                   {cons,Line,
                                    {tuple,Line,
                                     [{atom,Line,expression},
                                      {string,Line,CheckCallStr}]},
                                    {cons,Line,
                                     {tuple,Line,
                                      [{atom,Line,expected},{atom,Line,true}]},
                                     {cons,Line,
                                      {tuple,Line,
                                       [{atom,Line,value},
                                        {'case',Line,
                                         {var,Line,'__V'},
                                         [{clause,Line,
                                           [{atom,Line,false}],
                                           [],
                                           [{var,Line,'__V'}]},
                                          {clause,Line,
                                           [{var,Line,'_'}],
                                           [],
                                           [{tuple,Line,
                                             [{atom,Line,not_a_boolean},
                                              {var,Line,'__V'}]}]}]}]},
                                      {nil,Line}}}}}}}]}]}]}]}]}]}},
                    []}]}]}]}}]}]}]}]}.

add_eunit([Form|[]], Eunit) ->
    [Form|Eunit];
add_eunit([Form|Rest], Eunit) ->
    [Form|add_eunit(Rest,Eunit)];
add_eunit([],_Eunit ) ->
    [].


t_eunit_form({function, L, Name, 0, _Cs}, S, PropPrefix, Opts) ->
    N = atom_to_list(Name) ,
    case lists:prefix(PropPrefix, N) of
        true ->
            Assertion = assertion(N, L, Opts),
            sets:add_element(Assertion, S);
        false ->
            S
    end;
t_eunit_form(_, S, _, _) ->
    S.

t_rewrite([{attribute,_,module,{Name,_Ps}}=M | Fs], Exports) ->
    module_decl(Name, M, Fs, Exports);
t_rewrite([{attribute,_,module,Name}=M | Fs], Exports) ->
    module_decl(Name, M, Fs, Exports);
t_rewrite([F | Fs], Exports) ->
    [F | t_rewrite(Fs, Exports)];
t_rewrite([], _Exports) ->
    [].            % fail-safe, in case there is no module declaration

rewrite([{function,_,?CHECK,0,_}=F | Fs], As, Module, _GenQC) ->
    rewrite(Fs, [F | As], Module, false);
rewrite([F | Fs], As, Module, GenQC) ->
    rewrite(Fs, [F | As], Module, GenQC);
rewrite([], As, Module, GenQC) ->
    {if GenQC ->
             [{function,0,?CHECK,0,
               [{clause,0,[],[],
                 [{call,0,{remote,0,{atom,0,triq},{atom,0,check}},
                   [{atom,0,Module}]}]}]}
              | As];
        true ->
             As
     end,
     GenQC}.

module_decl(Name, M, Fs, Exports) ->
    Module = Name,
    {Fs1, GenQC} = rewrite(Fs, [], Module, true),
    Es = if GenQC -> [{?CHECK,0} | Exports];
            true -> Exports
         end,
    [M, {attribute,0,export,Es} | lists:reverse(Fs1)].
