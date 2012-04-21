-module(zm_ch_base).
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

handle_cast({msg, Msg}, S) ->
  ChannelId = S#state.id,
  Send = fun
    ({SubPid, SubscriptionRef, []}, {Sent, Filtered, Error}) ->
      SubPid ! {msg, {ChannelId, SubscriptionRef}, Msg},
      {Sent + 1, Filtered, Error};
    ({SubPid, SubscriptionRef, [{filter, Filter}]}, {Sent, Filtered, Error}) ->
      case Filter(Msg) of
        true ->
          SubPid ! {msg, {ChannelId, SubscriptionRef}, Msg},
          {Sent + 1, Filtered, Error};
        _ ->
          {Sent, Filtered + 1, Error}
      end;
    ({_SubPid, _SubscriptionRef, _}, {Sent, Filtered, Error}) ->
      {Sent, Filtered, Error + 1}
  end,
  {_Sent, _Filtered, _Error} = zm_sub_mon:fold_subs(ChannelId, {0, 0, 0}, Send),
  {noreply, S};

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

handle_info({From, ping}, State) ->
  From ! {self(), pong},
  {noreply, State};

handle_info(crash, State) ->
  P = 1 / 0, % crash this process
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, S) ->
  gen_event:notify(zm_em_sys, {"INFO", ?MODULE, io_lib:format("channel ~p terminated. Reason: ~p", [S#state.id, Reason])}).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
