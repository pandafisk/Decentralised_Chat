-module(test).

-compile(export_all).

-record(msg, {date, name, message}).

init() ->
    mnesia:start(),
    mnesia:create_schema([]).

%% ------------------------------------------
%% FUNCTIONS FOR DEALING WITH COMMUNICATION
%% ------------------------------------------

%% A function for creating a new group.
new_group(Name) ->
    mnesia:create_table(Name, [
        {attributes, record_info(fields, msg)}, 
        {record_name, msg},
        {type, ordered_set}]).


%% A function for joining an existing group.
join(Group, User) ->
    %message = io_lib:format("~p joined the  group!", [User),
    Users = findUniqueUsers(Group),
    {_, UserName} = User,
    case lists:member(UserName, Users) of
        true ->
            io:format("You are already a member of this group.\n");
        false ->
            Message = "Joined the group!",
            Insert = fun() -> 
                mnesia:write(Group, #msg{
                    date = erlang:timestamp(),
                    name = User,
                    message = Message}, write)
            end,
            NewMessage = lists:flatten(io_lib:format("(~p - ~p): ~p", [Group, User, Message])),
            send(Users, NewMessage),
            mnesia:transaction(Insert)
    end.


%% A functions that sends a message to the group.
sendMsg(Group, User, Message) ->
    Users = findUniquePIDs(Group),
    {PID, _} = User,
    case lists:member(PID, Users) of
        true ->
            Insert = fun() -> 
                mnesia:write(Group, #msg{
                    date = erlang:timestamp(),
                    name = User,
                    message = Message}, write)
            end,
            NewMessage = lists:flatten(io_lib:format("(~p - ~p): ~p", [Group, User, Message])),
            send(Users, NewMessage),
            mnesia:transaction(Insert);
        false ->
            io:format("Please join the group: ~p, before sending a message.\n", [Group])
    end.



%% ------------------------------------------
%% FUNCTIONS FOR DATABASE INTERACTION
%% ------------------------------------------

msg_history(Group) ->
    History = search_history(Group),
    lists:reverse(History).

%% A function for displaying message history of a group.
search_history(Table_name)->
    Iterator =  fun(Rec,_)->
                    {_, Time, Name, Msg} = Rec,
                    self() ! Rec,
                    NewMessage = lists:flatten(io_lib:format("~p: ~p - ~p (~p)~n",[Table_name, UserName, Msg, Time])),
                    [NewMessage|Arr]
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.


%% A function for listing users in a group.
findUniqueUsers(Group) ->
    Users = findUserNames(Group),
    uniques(Users).

findUniquePIDs(Group) ->
    Users = findUserPIDs(Group),
    uniques(Users).


%% A function for finding a User, based on Username.
findUser(Group, User) ->
    Users = findUserNames(Group),
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
findUserNames(Table_name) ->
    Iterator =  fun(Rec,Arr)->
                    {_, _, Name, _} = Rec,
                    {_, UserName} = Name,
                    [UserName|Arr]
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.

% Find all users that are not unique
findUserPIDs(Table_name) ->
    Iterator =  fun(Rec,Arr)->
                    {_, _, Name, _} = Rec,
                    {PID, _} = Name,
                    [PID|Arr]
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
