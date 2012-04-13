-module(zm_channel).

-vsn("1.0").
-author("czinkos@gmail.com").

%% API functions
-export([start/3, stop/1, list/0, subscribe/3, unsubscribe/1 ]).


-define(CHILD_SPEC(ChannelModule, ChannelId, Options), 
    {
      ChannelId,
      {ChannelModule, start_link, [ChannelId, Options]},
      transient,
      5000,
      worker,
      [ChannelModule]
    }
).

%%% parameter with channel process name, and module name
start(ChannelModule, ChannelId, Options) ->
  case zm_channel_sup:start_channel(?CHILD_SPEC(ChannelModule, ChannelId, Options)) of
    {ok, _Pid} ->
      gen_event:notify(zm_em_sys, {"INFO", ?MODULE, "channel '" ++ erlang:atom_to_list(ChannelId) ++ "' started" }),
      {ok, started};
    R ->
      {error, cannotstart, R}
  end.

list() ->
  zm_channel_sup:list_channels().

stop(Channel) ->
  zm_sub_mon:deregister_channel(Channel),
  zm_channel_sup:stop_channel(Channel),
  gen_event:notify(zm_em_sys, {"INFO", ?MODULE, "channel '" ++ erlang:atom_to_list(Channel) ++ "' stopped" }).

subscribe(Channel, Pid, Options) ->
  zm_sub_mon:register(Channel, Pid, Options).

unsubscribe(SubscriptionRef) ->
  zm_sub_mon:deregister(SubscriptionRef).

