-module(chat_server).

-behaviour(gen_server).

-export([start_link/0, start/1]).
%%Gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%Public
-export([newGroup/1, joinGroup/2, sendMessage/3, findUser/2, findGroup/1, users/1, history/1]).


-record(state, {}).

%% ============================================
%%          Client Call
%% ============================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(Name) ->
  gen_server:start({local, Name}, ?MODULE, [], []).

newGroup(Group) ->
    gen_server:call(?MODULE, {new_group, Group}).

joinGroup(Group, User) ->
    gen_server:call(?MODULE, {join, Group, User}).

sendMessage(Group, User, Message) ->
    gen_server:call(?MODULE, {sendMessage, Group, User, Message}).

findUser(Group, User) ->
    gen_server:call(?MODULE, {findUser, Group, User}).

findGroup(Group) ->
    gen_server:call(?MODULE, {findGroup, Group}).

users(Group) ->
    gen_server:call(?MODULE, {users, Group}).

history(Table) ->
    gen_server:call(?MODULE, {history, Table}).

%% ============================================
%%          Call Back Functions
%% ============================================

init(_Args) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) starting...~n", [{local, ?MODULE}, self()]),
    test:init(),
    {ok, #state{}}.

handle_call({new_group, Group}, _From, State) ->
    Get = test:new_group(Group),
    io:format("~p (~p) User: ~p Exists.~n",[?MODULE, self(), Get]),
    {reply, ok, State};

handle_call({join, Group, User}, _From, State) ->
    Get = test:join(Group, User),
    io:format("~p (~p) User: ~p Exists.~n",[?MODULE, self(), Get]),
    {reply, ok, State};

handle_call({sendMessage, Group, User, Message}, _From, State) ->
    Get = test:sendMsg(Group, User, Message),
    io:format("~p (~p) User: ~p Exists.~n",[?MODULE, self(), Get]),
    {reply, ok, State};

handle_call({findUser, Group, User}, _From, State) ->
    Get = test:findUser(Group, User),
    io:format("~p (~p) User: ~p Exists.~n",[?MODULE, self(), Get]),
    {reply, ok, State};

handle_call({findGroup, Group}, _From, State) ->
    Get = test:findGroup(Group),
    io:format("~p (~p) Group: ~p Exists.~n",[?MODULE, self(), Get]),
    {reply, ok, State};

handle_call({users, Group}, _From, State) ->
    Users = test:findUniqueUsers(Group),
    io:format("~p (~p) Users: ~p~n",[?MODULE, self(), Users]),
    {reply, Users, State};

handle_call({history, Table}, _From, State) ->
    Chat = test:msg_history(Table),
    io:format("~p (~p) Messages: ~p~n",[?MODULE, self(), Chat]),
    {reply, Chat, State};


handle_call( _Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
