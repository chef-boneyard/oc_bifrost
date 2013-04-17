-module(bql_conn_sup).

-behaviour(supervisor).

-export([start_link/0,
         new_fsm/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_fsm(Conn) ->
    {ok, Pid} = bql_conn_fsm:start_link(Conn),
    ok = bql_conn_fsm:takeover_socket(Pid),
    {ok, Pid}.

init([]) ->
    {ok, {{simple_one_for_one, 3, 60},
          [{bql_conn_fsm,
            {bql_conn_fsm, start_link, []},
            temporary, 1000, worker, [bql_conn_fsm]}]}}.
