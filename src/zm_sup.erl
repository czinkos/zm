-module(zm_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

start_link() ->
    start_link([]).
start_link(Options) ->
    supervisor:start_link(zm_sup, Options).

init(_Options) ->
  EventManager = {
    zm_em_sys,
    {gen_event, start_link, [{local, zm_em_sys}]},
    permanent,
    5000,
    worker,
    [gen_event]
  },
  ChannelSupervisor = {
    zm_channel_sup, % id
    {zm_channel_sup, start_link, []}, % start func
    permanent, % restart
    infinity, % shutdown time in case of supervisor
    supervisor, % child type
    [zm_channel_sup] % module
  },
  SubscriberMonitor = {
    zm_sub_mon, % id
    {zm_sub_mon, start_link, []}, % start func
    permanent, % restart
    5000, % shutdown time
    worker, % child type
    [zm_sub_mon] % module
  },
  Children = [EventManager, ChannelSupervisor, SubscriberMonitor],
  RestartStrategy = {one_for_one, 5, 10}, 
  {ok, {RestartStrategy, Children}}.

