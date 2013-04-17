Nonterminals
id expr perm perm_expr type typeof_expr target_list target_list_body.

Terminals
on_kw typeof_kw can_kw cannot_kw uuid create update read delete grant container object actor lend lstart comma user_kw.

Rootsymbol expr.

id -> uuid : uuid('$1').

perm -> create : perm('$1').
perm -> update : perm('$1').
perm -> read   : perm('$1').
perm -> delete : perm('$1').
perm -> grant  : perm('$1').

perm_expr -> can_kw perm : {positive, '$2'}.
perm_expr -> cannot_kw perm : {negative, '$2'}.

type -> container : type('$1').
type -> object    : type('$1').
type -> actor     : type('$1').

typeof_expr -> typeof_kw type : {type, '$2'}.

target_list -> id: ['$1'].
target_list -> lstart target_list_body lend: clean_up_list('$2').

target_list_body -> id: ['$1'].
target_list_body -> id comma target_list_body: ['$1'] ++ '$3'.

expr -> id perm_expr on_kw typeof_expr target_list: {ok, make_auth_req('$1', '$2', '$4', '$5')}.
expr -> user_kw id: {ok, make_user_context('$2')}.

Erlang code.
-export([string/1]).

-include("bql.hrl").

string(Text) ->
    {ok, Tokens, _} = bql_lexer:string(Text),
    case parse(Tokens) of
        {error, {Line, bql_parser, Error}} ->
            error_logger:error_msg("Parse error on ~p: ~s~n", [Line, Error]),
            {error, failed_parse};
        Result ->
            Result
    end.

%% Private functions
uuid({uuid, _, Value}) ->
    list_to_binary(Value).

perm({create, _, _}) ->
    create;
perm({read, _, _}) ->
    read;
perm({update, _, _}) ->
    update;
perm({delete, _, _}) ->
    delete;
perm({grant, _, _}) ->
    grant.

type({object, _, _}) ->
    object;
type({actor, _, _}) ->
    actor;
type({container, _, _}) ->
    container;
type({group, _, _}) ->
    group.

clean_up_list([H]) ->
    H;
clean_up_list(L) ->
    lists:flatten(L).

make_auth_req(Caller, {CheckType, Perm}, {type, EntryType}, Entries) ->
    #auth_req{caller=Caller, check_type=CheckType, perm=Perm, entry_type=EntryType, entries=Entries}.

make_user_context(Caller) ->
    #user_context{user=Caller}.
