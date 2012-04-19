-module(zm_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs)->
  CreateTable = zm_sub_mon:create_table(),
  start_server(CreateTable).

start_server(ok) ->
  start_server();
start_server(M) ->
  {error, M}.

start_server() ->
  case zm_sup:start_link() of
    {ok, Pid} ->
      gen_event:notify(zm_em_sys, {"INFO", ?MODULE, "zm_sup started"}),
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) ->
  ok.
