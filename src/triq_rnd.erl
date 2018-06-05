%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright 2018 Triq authors
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

-module(triq_rnd).

-export([ seed/0
        , seed/1
        , uniform/0
        , uniform/1
        , uniform_s/1
        , uniform_s/2
        ]).

-ifdef(HAVE_RAND_MOD).
seed()           -> rand:seed(exsplus).
seed({_,_,_}=S)  -> rand:seed(exsplus, S);
seed(Exp)        -> rand:seed(Exp).
uniform()        -> rand:uniform().
uniform(N)       -> rand:uniform(N).
uniform_s(St)    -> rand:uniform_s(St).
uniform_s(N, St) -> rand:uniform_s(N, St).
-else.
seed()           -> random:seed().
seed(Exp)        -> random:seed(Exp).
uniform()        -> random:uniform().
uniform(N)       -> random:uniform(N).
uniform_s(St)    -> random:uniform_s(St).
uniform_s(N, St) -> random:uniform_s(N, St).
-endif.
