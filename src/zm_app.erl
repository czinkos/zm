-module(zm_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs)->
  SchemaResult = mnesia:create_schema([node()]),
  CreateTable = zm_sub_mon:create_table(),
  start_server(SchemaResult, CreateTable).

% first time run
start_server(ok, ok) ->
  start_server();
start_server({error,{_,{already_exists,_}}}, ok) ->
  start_server();
start_server(_, M) ->
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
