-module(zm_channel_sup).
-behaviour(supervisor).

-export([
start_link/0, 
start_link/1, 
start_channel/1,
stop_channel/1,
list_channels/0
]).

-export([init/1]).

start_link() ->
  start_link([]).
start_link(Options) ->
  supervisor:start_link({local, zm_channel_sup}, zm_channel_sup, Options).

start_channel(ChildSpec) ->
  supervisor:start_child(?MODULE, ChildSpec).

stop_channel(Id) ->
  supervisor:terminate_child(?MODULE, Id),
  supervisor:delete_child(?MODULE, Id).

list_channels() ->
  [ Id || {Id, _Child, _, _} <- supervisor:which_children(?MODULE) ].

init(_Options) ->
  Children = [],
  RestartStrategy = {one_for_one, 5, 10}, 
  {ok, {RestartStrategy, Children}}.

