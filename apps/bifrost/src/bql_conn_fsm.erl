-module(bql_conn_fsm).

-behaviour(gen_fsm).

-include("bql.hrl").

%% API
-export([start_link/1,
         takeover_socket/1]).

%% gen_fsm callbacks
-export([init/1,
         handle_info/3,
         terminate/3,
         handle_event/3,
         handle_sync_event/4,
         code_change/4]).

%% States
-export([need_socket_ownership/3,
         need_user_context/2,
         ready/2]).

-define(SERVER, ?MODULE).

-record(state, {socket,
                user}).

start_link(Socket) ->
    gen_fsm:start_link(?MODULE, [Socket], []).

takeover_socket(FsmPid) ->
    ok = gen_fsm:sync_send_event(FsmPid, takeover),
    ok.

init([Socket]) ->
    {ok, state_name, #state{socket=Socket}}.

need_user_context({input, Data}, #state{socket=Socket}=State) ->
    case bql_parser:string(Data) of
        {ok, #user_context{user=User}} ->
            send_ok(Socket),
            {next_state, ready, State#state{user=User}};
        {ok, _} ->
            send_error(Socket, invalid_command),
            {next_state, need_user_context, State};
        _Error ->
            send_error(Socket, syntax_error),
            {next_state, need_user_context, State}
    end.

ready({input, Data}, #state{user=User, socket=Socket}=State) ->
    case bql_parser:string(Data) of
        {ok, #user_context{user=User}} ->
            send_ok(Socket),
            {next_state, ready, State#state{user=User}};
        {ok, _} ->
            send_error(Socket, syntax_error),
            {next_state, ready, State};
        _Error ->
            Data1 = lists:flatten([User, " ", Data]),
            case bql_parser:string(Data1) of
                {ok, #auth_req{}=AuthReq} ->
                    process_query(AuthReq, Socket);
                _Error1 ->
                    send_error(Socket, syntax_error)
            end,
            {next_state, ready, State}
    end.

need_socket_ownership(takeover, _From, #state{socket=Socket}=State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {reply, ok, need_user_context, State}.

handle_info({tcp, _Socket, Data}, StateName, State) ->
    gen_fsm:send_event(self(), {input, Data}),
    {next_state, StateName, State};
handle_info({tcp_closed, _Socket}, StateName, State) ->
    {stop, shutdown, State};
handle_info({tcp_error, _Socket, _Reason}, StateName, State) ->
    {stop, shutdown, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ignored, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
send_error(Socket, invalid_command) ->
    send_reply(Socket, "ERR:Invalid command\n");
send_error(Socket, execution_error) ->
    send_reply(Socket, "ERR:Execution error\n");
send_error(Socket, syntax_error) ->
    send_reply(Socket, "ERR:Syntax error\n").

send_ok(Socket) ->
    send_reply(Socket, "OK:\n").
send_true(Socket) ->
    send_reply(Socket, "OK:1\n").
send_false(Socket) ->
    send_reply(Socket, "OK:0\n").

send_reply(Socket, Reply) ->
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}]).

process_query(#auth_req{caller=Caller, check_type=CheckType,
                        perm=Perm, entry_type=EntryType, entries=[]}, Socket) ->
    send_true(Socket),
    ok;
process_query(#auth_req{caller=Caller, check_type=CheckType,
                        perm=Perm, entry_type=EntryType, entries=[Target|T]}=AR, Socket) ->
    case bifrost_db:has_permission(EntryType, Target, Caller, Perm) of
        {ok, Answer} ->
            if
                Answer == true andalso CheckType == positive ->
                    process_query(AR#auth_req{entries=T}, Socket);
                Answer == false andalso CheckType == negative ->
                    process_query(AR#auth_req{entries=T}, Socket);
                true ->
                    send_false(Socket)
            end;
        Error ->
            send_error(Socket, execution_error)
    end.
