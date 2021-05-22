# Decentralized Chat

This is a Distributed and decentralized chat, that supports multiple groups

## Instructions

To run the code, compile the files `chat_client`, `chat_server`, `chat_supervisor` & `db_logic` respectively.

To start the chat service, start erlang with a name and set cookies, and run `chat_client:start_link()`:

```erlang
> erl -sname alice -cookies 1234

(alice@DESKTOP-BCH0NID)1> chat_client:start_link().

{local,chat_supervisor} (<0.86.0>) starting... 
{local,chat_server} (<0.87.0>) starting...     
true
```

To connect another peer, start erlang with a name, and the same cookie, and run `chat_client:addNode(Host)`, where `Host` is any running peer (note this gives an error `{aborted,{already_exists,schema,'bob@DESKTOP-BCH0NID'}}`, but running `mnesia:info()` confirms all is as should be):

```erlang
> erl -sname bob -cookies 1234

(bob@DESKTOP-BCH0NID)1> chat_client:addNode('alice@DESKTOP-BCH0NID').

{local,chat_supervisor} (<0.103.0>) starting... 
{local,chat_server} (<0.104.0>) starting...
[{aborted,{already_exists,schema,'bob@DESKTOP-BCH0NID'}}]
```

**From any running peer** you can interact with the service via the following functions in `chat_client`:

- `addNode/1`: Add local node to the cluster if the chat has been started. i.e. this is *not* called from the node that initialises the chat-service. (`Host` must be a running peer)
- `createGroup/1`: Creates a new group with the given name
- `sendMessage/2`: Sends a message to the given group
- `findUser/2`: Checks whether the user, `User` exists in the given group
- `findGroup/1`: Checks whether the group, `Group` exists
- `listUsers/1`: Lists all users in the given group
- `viewHistory/1`: Lists the chat history for the given group

## Examples

(After compiling as described)

### Starting the chat and connecting another peer

`shell 1`

```erlang
> erl -sname alice -cookies 1234

(alice@DESKTOP-BCH0NID)1> chat_client:start_link().

{local,chat_supervisor} (<0.86.0>) starting... 
{local,chat_server} (<0.87.0>) starting...     
true
```

`shell 2`

```erlang
> erl -sname bob -cookies 1234

(bob@DESKTOP-BCH0NID)1> chat_client:addNode('alice@DESKTOP-BCH0NID').

{local,chat_supervisor} (<0.103.0>) starting... 
{local,chat_server} (<0.104.0>) starting...
[{aborted,{already_exists,schema,'bob@DESKTOP-BCH0NID'}}]
```

### Creating a group

```erlang
(alice@DESKTOP-BCH0NID)2> chat_client:createGroup(birds). 
chat_server (<0.87.0>) The group birds has been created.
ok
```

### Sending a message to a group

Sending a message will result in the chat-history being shown to any peers, which have sent a message to the given group.

`shell 1`

```erlang
(alice@DESKTOP-BCH0NID)3> chat_client:sendMessage(birds, "Hello").

chat_server (<0.87.0>) You have sent your message to all users in birds.
chat_server (<0.87.0>) Users: ['alice@DESKTOP-BCH0NID']
---USER:'alice@DESKTOP-BCH0NID'
---Group Name:birds
birds: 'Alice' - "Hello" sent in "2021-05-22 13:27:25"
chat_server (<0.87.0>) Messages: ok
---END OF CHAT HISTORY--
ok
```

`shell 2`

```erlang
(bob@DESKTOP-BCH0NID)3> chat_client:sendMessage(birds, "Hello to you too").

chat_server (<0.104.0>) You have sent your message to all users in birds.
chat_server (<0.104.0>) Users: ['bob@DESKTOP-BCH0NID','alice@DESKTOP-BCH0NID']
---USER:'bob@DESKTOP-BCH0NID'
---Group Name:birds
birds: 'Alice' - "Hello" sent in "2021-05-22 13:27:25" 
birds: 'Bob' - "Hello to you too" sent in "2021-05-22 13:31:14"     
chat_server (<0.104.0>) Messages: ok
---USER:'alice@DESKTOP-BCH0NID'
---Group Name:birds
---END OF CHAT HISTORY--
ok
```

The message sent from bob, will result in the chat-history being shown to `alice`:

```erlang
(alice@DESKTOP-BCH0NID)4> birds: 'Alice' - "Hello" sent in "2021-05-22 13:27:25"
(alice@DESKTOP-BCH0NID)4> birds: 'Bob' - "Hello to you too" sent in "2021-05-22 13:31:14"
(alice@DESKTOP-BCH0NID)4> chat_server (<0.87.0>) Messages: ok
```

### Searching for a user

```erlang
(alice@DESKTOP-BCH0NID)4> chat_client:findUser(birds, 'bob@DESKTOP-BCH0NID').
chat_server (<0.87.0>) User: 'bob@DESKTOP-BCH0NID' Exists. 
ok

(alice@DESKTOP-BCH0NID)5> chat_client:findUser(birds, 'non-existing').
chat_server (<0.87.0>) User: 'non-existing' Does not exist. 
ok
```

### Searching for a group

```erlang
(alice@DESKTOP-BCH0NID)6> chat_client:findGroup(birds). 
chat_server (<0.87.0>) Group: birds Exists.
ok

(alice@DESKTOP-BCH0NID)7> chat_client:findGroup('non-existing'). 
chat_server (<0.87.0>) Group: 'non-existing' Does not exist.
ok
```

### Listing users in a group

```erlang
(alice@DESKTOP-BCH0NID)8> chat_client:listUsers(birds). 

chat_server (<0.87.0>) Users: ['bob@DESKTOP-BCH0NID','alice@DESKTOP-BCH0NID']
['bob@DESKTOP-BCH0NID','alice@DESKTOP-BCH0NID']
```

### Viewing chat-history for a group

```erlang
(alice@DESKTOP-BCH0NID)9> chat_client:viewHistory(birds).

birds: 'Alice' - "Hello" sent in "2021-05-22 13:27:25" 
birds: 'Bob' - "Hello to you too" sent in "2021-05-22 13:31:14"     
chat_server (<0.87.0>) Messages: ok
ok
```
