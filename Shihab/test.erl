-module(test).
-import(lists,[nth/2]).
-import(string,[lexemes/2, titlecase/1]).
-compile(export_all).

-record(msg, {date, name, message}).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start().
%% ------------------------------------------
%% FUNCTIONS FOR DEALING WITH COMMUNICATION
%% ------------------------------------------

%% A function for creating a new group.
new_group(Name) ->
    % mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(Name, [
        {attributes, record_info(fields, msg)},
        {disc_copies, [node()]}, 
        {record_name, msg},
        {type, ordered_set}]).


%% A function for joining an existing group.
join(Group, User) ->
    %message = io_lib:format("~p joined the  group!", [User),
    Message = "Joined the group!",
    sendMsg(Group, User, Message).


%% A functions that sends a message to the group.
sendMsg(Group, User, Message) ->
    Insert = fun() -> 
        mnesia:write(Group, #msg{
            date = erlang:timestamp(),
            name = User,
            message = Message}, write)
    end,
    Users = findUniques(Group),
    % {_, _, Users} = fun() -> 
    %     mnesia:match_object(Group, {'_', '_', '_'}, read)
    % end,
    NewMessage = lists:flatten(io_lib:format("(~p - ~p): ~p", [Group, User, Message])),
    % send(Users, NewMessage),
    mnesia:transaction(Insert).



%% ------------------------------------------
%% FUNCTIONS FOR DATABASE INTERACTION
%% ------------------------------------------

%% A function for displaying message history of a group.
msg_history(Table_name)->
    Iterator =  fun(Rec,_)->
                    {_, Time, Name, Msg} = Rec,
                    self() ! Rec,
                    % Please see
                    Node = atom_to_list(Name),
                    Seperator = "@",
                    List = lexemes(Node, Seperator),
                    NodeName = list_to_atom(titlecase(nth(1, List))),
                    {{Y,M,D},{ H,MM,SS}} = calendar:now_to_datetime(Time),
                    Timestamp = lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, M, D,H,MM,SS])),
                    io:format("~p: ~p - ~p sent in ~p ~n",[Table_name, NodeName, Msg, list_to_atom(Timestamp)])
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.


%% A function for listing users in a group.
findUniques(Group) ->
    Users = findUsers(Group),
    uniques(Users).


%% A function for finding a User, based on Username.
findUser(Group, User) ->
    Users = findUsers(Group),
    lists:member(User, Users).


%% A function for finding a Group, based on Group name.
findGroup(Group) ->
    Tabs = mnesia:system_info(local_tables),
    {L,_} = lists:split(length(Tabs)-1, Tabs),
    lists:member(Group, L).

%% ------------------------------------------
%% LOCAL FUNCTIONS
%% ------------------------------------------

% Find all users that are not unique
findUsers(Table_name) ->
    Iterator =  fun(Rec,Arr)->
                    {_, _, Name, _} = Rec,
                    [Name|Arr]
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.

%% Send a message to all users
send([PID | Rest], MSG) ->
    PID ! MSG,
    send(Rest, MSG);
send([], _) ->
    io:format("done·~n").


%% Find unique elements in a list
uniques(L) ->
    uniques(L, [], []).

uniques([], _, Acc) ->
    lists:reverse(Acc);
uniques([X | Rest], Seen, Acc) ->
    case lists:member(X, Seen) of
        true -> uniques(Rest, Seen, Acc);
        false -> uniques(Rest, [X | Seen], [X | Acc])
    end.


    addReplica(NodeName) ->
        rpc:call(NodeName, test, restartReplica, []),
        mnesia:change_config(extra_db_nodes, [NodeName]),
        mnesia:change_table_copy_type(schema, NodeName, disc_copies),
        Tabs = mnesia:system_info(tables),
        [mnesia:add_table_copy(H, 'adam@DESKTOP-6ECBQGE', disc_copies) || H <- Tabs].
    
    %% Removes a given node from the cluster of active server-nodes
    removeReplica(Nodename) ->
        rpc:call(Nodename, mnesia, stop, []),
        mnesia:del_table_copy(schema, Nodename).
    
    %% Restarts a node as a server, in case of a shut-down
    restartReplica() ->
        mnesia:start(),
        kv_db_supervisor:start_link_from_shell().