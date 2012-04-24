-module(zm_channel).
-vsn("1.0").
-author("czinkos@gmail.com").

%% API functions
-export([start/3, do_start/3, stop/1, do_stop/1, list/0, subscribe/3, unsubscribe/1, send/2, send_local/2]).

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
do_start(ChannelModule, ChannelId, Options) ->
  case zm_channel_sup:start_channel(?CHILD_SPEC(ChannelModule, ChannelId, Options)) of
    {ok, _Pid} ->
      gen_event:notify(zm_em_sys, {"INFO", ?MODULE, "channel '" ++ erlang:atom_to_list(ChannelId) ++ "' started" }),
      {ok, started};
    R ->
      {error, cannotstart, R}
  end.

start(ChannelModule, ChannelId, Options) ->
  rpc:multicall(?MODULE, do_start, [ChannelModule, ChannelId, Options]).

list() ->
  zm_channel_sup:list_channels().

do_stop(Channel) ->
  zm_sub_mon:deregister_channel(Channel),
  zm_channel_sup:stop_channel(Channel),
  gen_event:notify(zm_em_sys, {"INFO", ?MODULE, "channel '" ++ erlang:atom_to_list(Channel) ++ "' stopped" }).

stop(Channel) ->
  rpc:multicall(?MODULE, do_stop, [Channel]).

subscribe(Channel, Pid, Options) ->
  zm_sub_mon:register(Channel, Pid, Options).

unsubscribe(SubscriptionRef) ->
  zm_sub_mon:deregister(SubscriptionRef).

send(Channel, Message) ->
  gen_server:abcast(Channel, Message).

send_local(Channel, Message) ->
  gen_server:cast(Channel, Message).
