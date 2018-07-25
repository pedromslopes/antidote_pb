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
-module(antidotec_index).

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

-record(antidote_index, {
  index,
  updates,
  removedKeys
}).

-export_type([antidote_index/0]).
-opaque antidote_index() :: #antidote_index{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec new() -> antidote_index().
new() ->
  #antidote_index{index = gb_trees:empty(), updates = [], removedKeys = []}.

-spec new(orddict:orddict() | gb_trees:tree()) -> antidote_index().
new(Value) when is_list(Value) ->
  #antidote_index{index = gb_trees:from_orddict(Value), updates = [], removedKeys = []};
new(Value) ->
  #antidote_index{index = Value, updates = [], removedKeys = []}.

-spec value(antidote_index()) -> [term()].
value(#antidote_index{index = Index}) -> gb_trees:to_list(Index).

-spec dirty_value(antidote_index()) -> [term()].
dirty_value(#antidote_index{index = Index}) -> gb_trees:to_list(Index).

-spec update(term(), antidote_index()) -> antidote_index().
update({_Key, _Op} = Update, #antidote_index{updates = Updates} = Index) ->
  Index#antidote_index{updates = lists:append(Updates, [Update])}.

-spec remove(term(), antidote_index()) -> antidote_index().
remove(Key, #antidote_index{removedKeys = RemKeys} = Map) ->
  Map#antidote_index{removedKeys = lists:append(RemKeys, [Key])}.

to_ops(BoundObject, #antidote_index{updates = Updates, removedKeys = RemKeys}) ->
  R = case length(RemKeys) > 0 of
        true -> [{BoundObject, remove, RemKeys}];
        false -> []
      end,
  case length(Updates) > 0 of
    true -> [{BoundObject, update, Updates} | R];
    false -> R
  end.

-spec is_type(term()) -> boolean().
is_type(T) -> is_record(T, antidote_index).

-spec type() -> index.
type() -> index.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-endif.