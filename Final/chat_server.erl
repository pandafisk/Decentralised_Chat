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
    db_logic:init(),
    {ok, #state{}}.

handle_call({new_group, Group}, _From, State) ->
    db_logic:new_group(Group),
    % io:format("~p (~p) The group named '~p' has been created.~n",[?MODULE, self(), Group]),
    io:format("The group named '~p' has been created.~n",[Group]),
    {reply, ok, State};

handle_call({sendMessage, Group, User, Message}, _From, State) ->
    db_logic:sendMsg(Group, User, Message),
    % io:format("~p (~p) You have sent your message to all users in ~p.~n",[?MODULE, self(), Group]),
    io:format("You have sent your message to all users in group named '~p'. ~n",[Group]),
    {reply, ok, State};

handle_call({findUser, Group, User}, _From, State) ->
    case db_logic:findUser(Group, User) of
        % true -> io:format("~p (~p) User: ~p Exists. ~n",[?MODULE, self(), User]);
        true -> io:format("User named '~p' exists. ~n",[User]);
        % false -> io:format("~p (~p) User: ~p Does not exist. ~n",[?MODULE, self(), User])
        false -> io:format("User named '~p' DOES NOT exist! ~n",[User])
    end,
    {reply, ok, State};

handle_call({findGroup, Group}, _From, State) ->
    case db_logic:findGroup(Group) of
        % true -> io:format("~p (~p) Group: ~p Exists.~n",[?MODULE, self(), Group]);
        true -> io:format("Group named '~p' exists. ~n",[Group]);
        % false -> io:format("~p (~p) Group: ~p Does not exist.~n",[?MODULE, self(), Group])
        false -> io:format("Group named '~p' DOES NOT exist! ~n",[Group])
    end,
    {reply, ok, State};

handle_call({users, Group}, _From, State) ->
    Users = db_logic:findUniques(Group),
    % io:format("~p (~p) Users: ~p~n",[?MODULE, self(), Users]),
    io:format(" Users: ~p ~n",[Users]),
    {reply, Users, State};

handle_call({history, Table}, _From, State) ->
    Chat = db_logic:msg_history(Table),
    % io:format("~p (~p) Messages: ~p~n",[?MODULE, self(), Chat]),
    % io:format("Messages: ~p ~n",[Chat]),
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