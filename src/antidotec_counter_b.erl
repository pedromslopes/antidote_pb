%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(antidotec_counter_b).

-include_lib("riak_pb/include/antidote_pb.hrl").

-behaviour(antidotec_datatype).

-export([new/0, new/1,
         value/1,
         to_ops/2,
         is_type/1,
         dirty_value/1,
         type/0
        ]).

-export([increment/1,
         increment/2,
         decrement/1,
         decrement/2,
         transfer/2
        ]).

-record(counter_b, {
          value :: {list(), list()},
          increments :: list(),
          decrements :: list(),
          transfers :: list()
         }).

-export_type([antidotec_counter_b/0]).
-opaque antidotec_counter_b() :: #counter_b{}.

-spec new() -> antidotec_counter_b().
new() ->
  #counter_b{value = {[], []}, increments = [], decrements = []}.

-spec new(integer()) -> antidotec_counter_b().
new({Incs, Decs}) when is_list(Incs) andalso is_list(Decs) ->
  #counter_b{value = {Incs, Decs}, increments = [], decrements = []}.

-spec value(antidotec_counter_b()) -> integer().
value(#counter_b{value = Value}) -> Value.

-spec dirty_value(antidotec_counter_b()) -> integer().
dirty_value(#counter_b{value = Value}) -> Value.

%% @doc Increments the counter with 1 unit.
-spec increment(antidotec_counter_b()) -> antidotec_counter_b().
increment(Counter) ->
  increment({1, actor}, Counter).

%% @doc Increments the counter with Amount units.
-spec increment({integer(), term()}, antidotec_counter_b()) -> antidotec_counter_b().
increment({Amount, Actor}, #counter_b{increments = Incs} = Counter)
  when is_integer(Amount) andalso Amount > 0 ->
  Counter#counter_b{increments = lists:append(Incs, [{Amount, Actor}])}.

%% @doc Decrements the counter by 1.
-spec decrement(antidotec_counter_b()) -> antidotec_counter_b().
decrement(Counter) ->
  decrement({1, actor}, Counter).

%% @doc Decrements the counter by the passed amount.
-spec decrement(integer(), antidotec_counter_b()) -> antidotec_counter_b().
decrement({Amount, Actor}, #counter_b{decrements = Decs} = Counter)
  when is_integer(Amount) andalso Amount > 0 ->
  Counter#counter_b{decrements = lists:append(Decs, [{Amount, Actor}])}.

transfer({Amount, Dest, Actor}, #counter_b{transfers = Trans} = Counter)
  when is_integer(Amount) andalso Amount > 0 ->
  Counter#counter_b{transfers = lists:append(Trans, [{Amount, Dest, Actor}])}.

-spec is_type(term()) -> boolean().
is_type(T) ->
    is_record(T, counter_b).

type() -> counter_b.

to_ops(BoundObject, #counter_b{increments = Incs, decrements = Decs, transfers = Trans}) ->
  TransOps = [{BoundObject, transfer, {Amount, Dest, Actor}} || {Amount, Dest, Actor} <- Trans],
  DecOps = [{BoundObject, decrement, {Amount, Actor}} || {Amount, Actor} <- Decs],
  IncOps = [{BoundObject, increment, {Amount, Actor}} || {Amount, Actor} <- Incs],
  lists:flatten([DecOps, TransOps, IncOps]).
