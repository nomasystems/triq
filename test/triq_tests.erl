%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2010-2013 by Trifork
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

%%
%% This file contains some sample properties, which also
%% function as a very simple test suite for Triq itself.
%%

-module(triq_tests).

%% including this also auto-exports all properties
-include("triq.hrl").

%% use eunit
-include_lib("eunit/include/eunit.hrl").


boolean_test() ->
    Unique = fun ordsets:from_list/1,
    ?assertEqual([false, true], Unique(triq_dom:sample(bool()))).

prop_intrange() ->
    ?FORALL({X,Y},
            ?SUCHTHAT({XX,YY},
                      {int(), int()},
                      XX < YY),

            true = triq:counterexample( prop_intrange(X,Y))).

prop_intrange(X,Y) when X < Y ->
    ?FORALL(I, int(X,Y),
            ?WHENFAIL(io:format("Min=~p, Max=~p, I=~p~n", [X,Y,I]),
                      (I >= X) andalso (I =< Y))).

prop_pos_integer() ->
    ?FORALL(PosInt, pos_integer(), PosInt > 0).

prop_non_neg_integer() ->
    ?FORALL(NonNegInt, non_neg_integer(), NonNegInt >= 0).

prop_large_integer() ->
    ?FORALL(LargeInt, largeint(), erlang:is_integer(LargeInt)).

prop_append() ->
    ?FORALL({Xs,Ys},{list(int()),list(int())},
            ?TRAPEXIT(lists:reverse(Xs++Ys)
                      ==
                          lists:reverse(Ys) ++ lists:reverse(Xs))).

xprop_delete() ->
    ?FORALL(
       L,list(int()),
       ?IMPLIES(L /= [],
                ?FORALL(I,elements(L),
                        ?WHENFAIL(io:format("L=~p, I=~p~n", [L,I]),
                                  not lists:member(I,lists:delete(I,L)))))).

delete_test() ->
    false = triq:check(xprop_delete()).

inverse('<') -> '>=';
inverse('>') -> '=<';
inverse('==') -> '/=';
inverse('=:=') -> '=/=';
inverse('=/=') -> '=:=';
inverse('/=') -> '=='.

prop_binop() ->
    ?FORALL({A,B,OP}, {any(),any(),elements(['>','<','==','=:=','=/=','/='])},
            erlang:OP(A,B)
            ==
                begin
                    ROP = inverse(OP),
                    not  ( erlang:ROP(A,B) )
                end
           ).

prop_timeout() ->
    fails(
      ?FORALL(N,shrink_without_duplicates(choose(50,150)),
              ?TIMEOUT(100, timer:sleep(N) == ok))
     ).

prop_sized() ->
    ?FORALL(T, ?SIZED(S, {true, choose(0,S)}),
            (erlang:tuple_size(T) == 2)
            and
            begin {true, Int} = T, Int >= 0 end
           ).

prop_simple1() ->
    ?FORALL(V, [1,int(),3|4],
            begin [1,X,3|4]=V, is_integer(X) end ).

prop_simple2() ->
    ?FORALL(V, {}, V == {}).

prop_simple3() ->
    ?FORALL(V, atom(),
            ?IMPLIES(V /= '',
                     begin
                         [CH|_] = erlang:atom_to_list(V),
                         (CH >= $a) and (CH =< $z)
                     end)).

prop_suchthat() ->
    ?FORALL({X,Y},
            ?SUCHTHAT({XX,YY},
                      {int(),int()},
                      XX < YY),
            X < Y).

%% If an iteration of ?SUCHTHAT loop fails and we don't increase sample
%% size, we might not get enough variance to obtain a rewarding value.
xprop_suchthat_sample_size() ->
    ?FORALL(
       _,
       ?SUCHTHAT(
          X,
          int(),
          X > 10),
       false).

suchthat_sample_size_test() ->
    [11] = triq:counterexample(xprop_suchthat_sample_size()).

tuple_failure_test() ->
    false = check(?FORALL(T, {int()},
                          begin
                              {V} = T,
                              V > 0
                          end)).

list_shrink_test() ->
    %% test that a list shrinks to the empty list
    true = lists:all(fun(_) ->
                             [[]] == triq:counterexample(
                                       ?FORALL(_, list(int()), false)
                                      )
                     end, lists:seq(1,100)).

non_empty_list_shrink_test() ->
    ?assert(
       lists:all(
         fun(_) ->
                 [[0]] =:= triq:counterexample(
                           ?FORALL(_, non_empty(list(int())), false))
         end,
         lists:seq(1, 15))).

%list_shrink2_testx() ->
%    %% test that a list doesn't easily end in a local 'smallest counterexample'
%    true = lists:all(fun(_) ->
%                             [[]] = triq:counterexample(
%                                       ?FORALL(L, list(oneof([a,b])),
%                                               not is_pairs_list(L))
%                                      ),
%                             true
%                     end, lists:seq(1,100)).
%
%is_pairs_list([])      -> true;
%is_pairs_list([X,X|T]) -> is_pairs_list(T);
%is_pairs_list(_)       -> false.

oneof_test_() ->
    {
        timeout, 15,
        fun() ->
            [{X,Y}] = triq:counterexample(
                ?FORALL({X,Y},
                        ?SUCHTHAT({A,B},
                                  {oneof([int(),real()]),
                                   oneof([int(),real()])},
                                  A < B),
                        begin
                            %% io:format("{X,Y} = ~p~n", [{X,Y}]),
                            is_integer(X) == is_integer(Y)
                        end
                       )),
            %% One variable must be equal to 0 and absolute value of the other
            %% must not be greater than 1.
            %% Note: 0 == 0.0
            ?assert((X * Y == 0) and (X + Y /= 0) and (abs(X) + abs(Y) =< 1))
       end
    }.

%%
%% This test makes sure that X shrinks only to 3.
%%
oneof2_test() ->
    [X] = triq:counterexample(?FORALL(_, oneof([choose(3,7)]), false)),
    3 = X.

%%
%% Test that largeint() shrinks to 0
%%
largeint_shrink_test() ->
    ?assertEqual([0], triq:counterexample(?FORALL(_, largeint(), false))).

%%
%% Test that vector doesn't shrink the length
%%
vector_test() ->
    [L] = triq:counterexample(?FORALL(_, vector(4, choose(3,7)), false)),
    [3,3,3,3] = L.

choose_test_() ->
    [?_assertEqual([3], triq:counterexample(?FORALL(_, choose(3,7), false))),
     ?_assertEqual([7], triq:counterexample(?FORALL(I, choose(3,7), I < 7))),
     ?_assertEqual([3], triq:counterexample(?FORALL(_, choose(3,3), false))),
     ?_assertEqual([0], triq:counterexample(?FORALL(_, choose(-3,7), false))),
     ?_assertEqual([-3], triq:counterexample(?FORALL(_, choose(-7,-3), false))),
     ?_assertEqual([0], triq:counterexample(?FORALL(_, choose(-3,0), false))),
     ?_assertEqual([0], triq:counterexample(?FORALL(_, choose(0,3), false))),
     ?_assertEqual([1], triq:counterexample(?FORALL(_, choose(1,100), false))),
     ?_assertEqual([5], triq:counterexample(?FORALL(_, choose(5,1 bsl 80), false))),
     ?_assertException(error, function_clause, choose(7,3))].

%%
%% Test binary shrinking
%%
binary_test() ->
    [X] = triq:counterexample(?FORALL(_, binary(2), false)),
    <<0,0>> = X.

not_reach_rsn() ->
    ?LET(Rsn,choose(2,5),<<Rsn>>).

binary2_test() ->
    [<<2>>] = triq:counterexample(?FORALL(_, not_reach_rsn(), false)).

%%
%% Test shrinking of elements
%%
elements_test() ->
    [X] = triq:counterexample
            (?FORALL(_,
                     elements([one,two,three]),
                     false)),
    one = X.

suchthat_shrinking_test() ->
    ?assertEqual(
        [2],
        triq:counterexample(
          ?FORALL(
             _,
             ?SUCHTHAT(
                X,
                non_neg_integer(),
                X > 1),
             false))).

noshrink_test() ->
    noshrink_test(100).

noshrink_test(0) ->
    true;
noshrink_test(Size) ->
    {Dom, Val} = triq_dom:pick(triq_dom:noshrink(any()), Size),
    {ShrinkedDom, ShrinkedVal} = triq_dom:shrink(Dom, Val),
    ?assertEqual({ShrinkedDom, ShrinkedVal}, {Dom, Val}),
    noshrink_test(Size - 1).

%%
%% Test passing counterexamples to properties
%%
recheck_test_() ->
    Good = [[1,2], 1],
    Bad = [[1,1], 1],
    [?_assertEqual(true, triq:check(xprop_delete(), Good)),
     ?_assertEqual(false, triq:check(xprop_delete(), Bad))].

counterexample_test() ->
    Counterexample = triq:counterexample(xprop_delete()),
    ?assertEqual(false, triq:check(xprop_delete(), Counterexample)).


%% -------------------------------------------------------------------
%% Property Testing
%% -------------------------------------------------------------------

run_property_testing_test_() ->
    {timeout, 60, fun run_property_testing_case/0}.

run_property_testing_case() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = triq:module(?MODULE),
    erlang:group_leader(EunitLeader, self()),
    ?assert(Res).
