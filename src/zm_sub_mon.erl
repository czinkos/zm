-module(zm_sub_mon).
-vsn("1.0").
-author("czinkos@gmail.com").
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([create_table/0, register/3, deregister/1, deregister_channel/1, fold_subs/3]).

-record(zm_channel_sub, { id, channel, sub, options}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  gen_event:notify(zm_em_sys, {"INFO", ?MODULE, "init started"}),
  State = ets:new(zm_sub_monrefs, [ set, public, named_table ]),
  Monitor = fun(#zm_channel_sub{ sub=Pid }, _Acc) -> 
    case ets:lookup(State, Pid) of
      [] ->
        MonitorRef = erlang:monitor(process, Pid),
        ets:insert(State, { Pid, MonitorRef, 1 } );
      [{P, _M, _C}] ->
        ets:update_counter(State, P, [{3, 1}])
    end
  end,
  mnesia:activity( 
    transaction, 
    fun() -> 
      mnesia:foldl(Monitor, 0, zm_channel_sub)
    end
  ), 
  gen_event:notify(zm_em_sys, {"INFO", ?MODULE, "init finished"}),
  {ok, State}.

%%%%%%%%%%%%%%%%
%% API functions
%%%%%%%%%%%%%%%%
create_table() ->
  mnesia:delete_table(zm_channel_sub),
  case mnesia:create_table(zm_channel_sub, [{type, set}, {index, [#zm_channel_sub.sub, #zm_channel_sub.channel]}, {disc_copies, [node()]}, {attributes, record_info(fields, zm_channel_sub)}]) of
    {atomic,ok} -> ok;
    {aborted,{already_exists,zm_channel_sub}} -> ok;
    M -> {error, M}
  end.

register(Channel, Pid, Options) ->
  gen_server:call(?MODULE, {register, Channel, Pid, Options}).

deregister(SubscriptionRef) ->
  gen_server:call(?MODULE, {deregister, SubscriptionRef}).

deregister_channel(Channel) ->
  gen_server:call(?MODULE, {deregister_channel, Channel}).

fold_subs(Channel, Acc0, Fun) ->
  QH = qlc:q([ { Pid, SubscriptionRef, Options} || #zm_channel_sub{ channel = Ch, sub=Pid, id=SubscriptionRef,  options=Options } <- ets:table(zm_channel_sub), Ch == Channel ]),
  qlc:fold(Fun, Acc0, QH).

%% internal functions
do_register(State, Channel, Pid, Options) ->
  SubscriptionRef = erlang:make_ref(),
  mnesia:dirty_write(#zm_channel_sub{ id=SubscriptionRef, channel = Channel, sub=Pid, options=Options}),
  Channel ! {subscribe, SubscriptionRef, Pid, Options},
  
  case ets:lookup(State, Pid) of
    [] ->
      MonitorRef = erlang:monitor(process, Pid),
      ets:insert(State, { Pid, MonitorRef, 1 });
    [{P, _M, _Counter}] ->
      ets:update_counter(State, P, [{3, 1}])
  end,
  SubscriptionRef.

do_deregister(_State, [], _) ->
  do_nothing;
do_deregister(State, [H|T], Reason) ->
  do_deregister(State, H, Reason),
  do_deregister(State, T, Reason);
do_deregister(State, #zm_channel_sub{sub=Pid} = S, subscriber_died) ->
  delete_and_notify(S, subscriber_died),
  ets:delete(State, Pid);
do_deregister(State, #zm_channel_sub{sub=Pid} = S, Reason) ->
  delete_and_notify(S, Reason),
  case ets:lookup(State, Pid) of
    [] ->
      ok;
    [{ P, M, 1 }]  ->
      erlang:demonitor(M),
      ets:delete(State, P);
    [{ P, _M, _Counter}] ->
      ets:update_counter(State, P, [{3, -1}])
  end.

delete_and_notify(#zm_channel_sub{id = SubscriptionRef, channel = Channel, sub=Pid}, channel_stopped) ->
  mnesia:dirty_delete(zm_channel_sub, SubscriptionRef),
  Pid ! {channel_stopped, Channel, SubscriptionRef}; 
delete_and_notify(#zm_channel_sub{id = SubscriptionRef, channel = Channel, sub=Pid, options=Options}, Reason) ->
  mnesia:dirty_delete(zm_channel_sub, SubscriptionRef),
  Channel ! {Reason, SubscriptionRef, Pid, Options}. 

%%%%%%%%%%%%%%%
handle_call({register, Channel, Pid, Options}, _From, State) ->
  SubscriptionRef = do_register(State, Channel, Pid, Options),
  {reply, SubscriptionRef, State};

handle_call({deregister, SubscriptionRef}, _From, State) ->
  do_deregister(State, ets:lookup(zm_channel_sub, SubscriptionRef), unsubscribe),
  {reply, ok, State};

handle_call({deregister_channel, Channel}, _From, State) ->
  QH = qlc:q([ S || #zm_channel_sub{ channel=Ch } = S <- ets:table(zm_channel_sub), Ch == Channel ]),
  do_deregister(State, qlc:eval(QH), channel_stopped), 
  {reply, ok, State};

handle_call(_Info, _From, State) ->
  {noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, State) ->
  QH = qlc:q([ S || #zm_channel_sub{ sub=P } = S <- ets:table(zm_channel_sub), P == Pid ]),
  do_deregister(State, qlc:eval(QH), subscriber_died), 
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  gen_event:notify(zm_em_sys, {"INFO", ?MODULE, io_lib:format("terminated. Reason: ~p", [Reason])}).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
