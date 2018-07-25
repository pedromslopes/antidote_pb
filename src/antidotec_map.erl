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

%%%-------------------------------------------------------------------
%%% @author pedrolopes
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(antidotec_map).

-include_lib("riak_pb/include/antidote_pb.hrl").

-behaviour(antidotec_datatype).

%% API
-export([new/0,
         new/1,
         value/1,
         dirty_value/1,
         to_ops/2,
         is_type/1,
         type/0]).

-export([update/2,
         remove/2]).

-record(antidote_map, {
  dict,
  updates,
  removedKeys
}).

-export_type([antidote_map/0]).
-opaque antidote_map() :: #antidote_map{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec new() -> antidote_map().
new() ->
  #antidote_map{dict = dict:new(), updates = [], removedKeys = []}.

-spec new(list() | dict:dict()) -> antidote_map().
new(Value) when is_list(Value) ->
  #antidote_map{dict = dict:from_list(Value), updates = [], removedKeys = []};
new(Value) ->
  #antidote_map{dict = Value, updates = [], removedKeys = []}.

-spec value(antidote_map()) -> [term()].
value(#antidote_map{dict = Dict}) -> dict:to_list(Dict).

-spec dirty_value(antidote_map()) -> [term()].
dirty_value(#antidote_map{dict = Dict}) -> dict:to_list(Dict).

-spec update(term(), antidote_map()) -> antidote_map().
update({{_Key, _Type}, _Op} = Update, #antidote_map{updates = Updates} = Map) ->
  Map#antidote_map{updates = lists:append(Updates, [Update])}.

-spec remove(term(), antidote_map()) -> antidote_map().
remove(Key, #antidote_map{removedKeys = RemKeys} = Map) ->
  Map#antidote_map{removedKeys = lists:append(RemKeys, [Key])}.

to_ops(BoundObject, #antidote_map{updates = Updates, removedKeys = RemKeys}) ->
  R = case length(RemKeys) > 0 of
        true -> [{BoundObject, remove, RemKey} || RemKey <- RemKeys];
        false -> []
      end,
  case length(Updates) > 0 of
    true -> [{BoundObject, update, Updates} | R];
    false -> R
  end.

-spec is_type(term()) -> boolean().
is_type(T) -> is_record(T, antidote_map).

-spec type() -> map.
type() -> map.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-endif.