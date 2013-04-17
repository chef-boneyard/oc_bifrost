-module(bql_server).

-behaviour(gen_nb_server).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-export([sock_opts/0,
         new_connection/2]).

start_link(IpAddr, Port) ->
  gen_nb_server:start_link(?MODULE, IpAddr, Port, []).

init([]) ->
  {ok, []}.

handle_call(_Msg, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

sock_opts() ->
  [list, {active, false}, {packet, line},
   {nodelay, true}].

new_connection(Sock, State) ->
    {ok, _Pid} = bql_conn_sup:new_fsm(Sock),
    {ok, State}.
