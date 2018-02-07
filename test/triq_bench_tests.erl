%% Copyright (c) 2018 Krzysztof Jurewicz
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

-module(triq_bench_tests).

-include_lib("triq/include/triq.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(
   LET_PROF(Prof, FunCall, Assertions),
   begin
       _ = cprof:start(),
       _ = FunCall,
       _ = cprof:pause(),
       Prof = cprof:analyse(),
       _ = cprof:stop(),
       Assertions
   end).

-spec call_count(
        mfa(),
        {integer(),
         list(
           {Mod :: atom(),
            ModCallCount :: integer(),
            [{mfa(), integer()}]})})
                -> integer().
%% @doc Extract call count for a specific MFA from CProf results.
call_count({Mod, _, _}=MFA, {_, ResultsPerMod}) ->
    {_, _, ModResults} = lists:keyfind(Mod, 1, ResultsPerMod),
    proplists:get_value(MFA, ModResults, 0).

%% When looking for optimization opportunities in Triq, one can try to tighten
%% assertions in the tests below.

constant_singleton_vector_test() ->
    ?LET_PROF(
       Prof,
       triq:counterexample(
         ?FORALL(
            _,
            vector(1, return(foo)),
            false)),
       ?assertMatch(
          C when C =< 25000, call_count(
                               {triq_dom, shrink_list_members, 4}, Prof))).
