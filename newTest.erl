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
    sendMsg(Group, User, User).


%% A functions that sends a message to the group.
sendMsg(Group, User, Message) ->
    Insert = fun() -> 
        mnesia:write(Group, #msg{
            date = erlang:timestamp(),
            name = User,
            message = Message}, write)
    end,
    {_, _, _, Users} = fun() -> 
        mnesia:match_object({Group, '_', '_', '_'})
    end,
    newMessage = io_lib:format("(~p - ~p): ~p", [Group, User, Message]),
    send(Users, newMessage),
    mnesia:transaction(Insert).



%% ------------------------------------------
%% FUNCTIONS FOR DATABASE INTERACTION
%% ------------------------------------------

%% A function for displaying message history of a group.
msg_history(Table_name)->
    Iterator =  fun(Rec,_)->
                    {_, Time, Msg, Name} = Rec,
                    self() ! Rec,
                    io:format("~p: ~p - ~p (~p)~n",[Table_name, Name, Msg, Time])
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.


%% A function for listing users in a group.
users(Group) ->
    {_, _, _, Users} = fun() -> 
        mnesia:match_object({Group, '_', '_', '_'})
    end,
    Query = uniques(Users),
    mnesia:transaction(Query).


%% A function for finding a User, based on Username.
findUser(User) ->
    Query = fun() -> 
        mnesia:match_object({'_', '_', '_', User})
    end,
    mnesia:transaction(Query).


%% A function for finding a Group, based on Group name.
findGroup(Group) ->
    Query = fun() -> 
        mnesia:match_object({Group, '_', '_', '_'})
    end,
    mnesia:transaction(Query).




%% ------------------------------------------
%% LOCAL FUNCTIONS
%% ------------------------------------------

%% Send a message to all users
send([PID | Rest], msg) ->
    PID ! msg,
    send(Rest, msg).

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
