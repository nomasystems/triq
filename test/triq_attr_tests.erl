%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright 2018 Tuncer Ayaz
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

-module(triq_attr_tests).

-include_lib("triq/include/triq.hrl").

-triq({eunit, [{runs, 512}]}).

prop_append() ->
    ?FORALL({Xs,Ys},{list(int()),list(int())},
            lists:reverse(Xs++Ys)
            ==
            lists:reverse(Ys) ++ lists:reverse(Xs)).
