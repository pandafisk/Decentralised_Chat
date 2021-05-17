-module(chat_client).

%% Internal
-export([createGroup/1, joinGroup/2, sendMessage/3, viewHistory/1, listUsers/1, findUser/1, findGroup/1]).
%% Remote
-export([remote_createGroup/2, remote_joinGroup/3, remote_sendMessage/4, remote_viewHistory/2, remote_listUsers/2, remote_findUser/2, remote_findGroup/2]).
%% Helpers
-export([get_server/0, connect_client/1]).


%% ======================================================
%%               Internal Calls
%% ======================================================
%% These function can only be called internally from a server-node

%% Creates a new group with the name group.
createGroup(Group) ->
    chat_server:newGroup(Group).

%% Joins the group with the name Group.
joinGroup(Group, User) ->
    chat_server:joinGroup(Group, User).

%% Sends a message to all users in the group with name Group.
sendMessage(Group, User, Message) ->
    chat_server:sendMessage(Group, User, Message).

%% Checks if a user exists, by userame.
findUser(User) ->
    chat_server:findUser(User).

%% Checks if a group exists, by group name.
findGroup(Group) ->
    chat_server:findGroup(Group).

%% List all Users that is connected to the group
listUsers(Group) ->
    chat_server:users(Group).

%% Lists the history of the chats in the group
viewHistory(Table) ->
    chat_server:history(Table).

%% ======================================================
%%              Remote Calls
%% ======================================================
%% These functions are intended to be called from a remote client-node
%% Can also be called from a server-node

%% Remote version of createGroup.
remote_createGroup(Server, Group) ->
    rpc:call(Server, chat_client, createGroup, [Group]).

%% Remote version of joinGroup.
remote_joinGroup(Server, Group, User) ->
    rpc:call(Server, chat_client, joinGroup, [Group, User]).

%% remote version of sendMessage.
remote_sendMessage(Server, Group, User, Message) ->
    rpc:call(Server, chat_client, sendMessage, [Group, User, Message]).

%% Remote version of findUser.
remote_findUser(Server, User) ->
    rpc:call(Server, chat_client, findUser, [User]).

%% Remote version of findGroup.
remote_findGroup(Server, Group) ->
    rpc:call(Server, chat_client, findGroup, [Group]).

%% Remote version of listUsers.
remote_listUsers(Server, Group) ->
    rpc:call(Server, chat_client, listUsers, [Group]).

%% Remote version of viewHistory.
remote_viewHistory(Server, Group) ->
    rpc:call(Server, chat_client, viewHistory, [Group]).


%% ======================================================
%%              Helpers
%% ======================================================
%% Helper functions to connect a client to the server

%% Returns a running server to the client
connect_client(Host) ->
    rpc:call(Host, chat_client, get_server, []).

%% Selects a random node from one of the servers, to be used as dedicated server for a client
%% Works as a load-balancer
get_server() ->
    Servers = mnesia:table_info(test, where_to_write),
    lists:nth(rand:uniform(length(Servers)), Servers).
