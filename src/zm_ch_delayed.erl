-module(zm_ch_delayed).
-vsn("1.0").
-author("czinkos@gmail.com").

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, { id }).

start_link(ChannelId, Options) ->
    gen_server:start_link({local, ChannelId}, ?MODULE, [ChannelId] ++ Options, []).

init([ChannelId | _Options]) ->
  {ok, #state{ id=ChannelId} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast({msg, {_Timestamp, _} = Message}, #state{ id=ChannelId } = State) ->
  Send = fun
    ({SubPid, SubscriptionRef, []}, {Sent, Delayed, Error}) ->
      SubPid ! {msg, {ChannelId, SubscriptionRef}, Message},
      {Sent + 1, Delayed, Error};
    ({SubPid, SubscriptionRef, [{delay, Delay}]}, {Sent, Delayed, Error}) ->
      erlang:send_after(timer:minutes(Delay), SubPid, {msg, {ChannelId, SubscriptionRef}, Message}),
      {Sent, Delayed + 1, Error};
    ({_SubPid, _SubscriptionRef, _}, {Sent, Delayed, Error}) ->
      {Sent, Delayed, Error + 1}
  end,
  {_Sent, _Delayed, _Error} = zm_sub_mon:fold_subs(ChannelId, {0, 0, 0}, Send),
  {noreply, State};

handle_cast(_Info, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% subscription
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info({subscribe, _SubscriptionRef, _SubPid, _Options}, State) ->
  {noreply, State};
handle_info({unsubscribe, _SubscriptionRef, _SubPid, _Options}, State) ->
  {noreply, State};
handle_info({subscriber_died, _SubscriptionRef, _SubPid, _Options}, State) ->
  {noreply, State};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(stop, State) ->
  {stop, normal, State};

handle_info(crash, State) ->
  io:format("Won't crash.\n"),
  {noreply, State};

handle_info({From, ping}, State) ->
  From ! pong,
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, _S) ->
  io:format("<<<<<<<<<<< Channel terminated. Reason: ~p >>>>>>>>>>>>\n", [Reason]),
  terminate.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
