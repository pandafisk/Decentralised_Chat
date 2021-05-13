-module(test).

-compile(export_all).

-record(msg, {date, name, message}).

init() ->
    mnesia:start(),
    mnesia:create_schema([]).


new_group(Name) ->
    mnesia:create_table(Name, [
        {attributes, record_info(fields, msg)}, 
        {record_name, msg},
        {type, ordered_set}]).

put(Table, Name, Message) ->
    Insert = fun() -> 
        mnesia:write(Table, #msg{
            date = erlang:timestamp(),
            name = Name,
            message = Message}, write)
    end,
   mnesia:transaction(Insert).


msg_history(Table_name)->
    Iterator =  fun(Rec,_)->
                    {_, Name, Msg, Time} = Rec,
                    self() ! Rec,
                    io:format("~p: ~p - ~p~n",[Name, Msg, Time])
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.