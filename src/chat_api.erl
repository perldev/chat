-module(chat_api).

-include("erws_console.hrl")
-export([last/1,
         get_from_reverse/4, 
         put_new_message/2,
	 create_store/1,
         create_store/2,
         system_message/2,
         get_last_count/4,
         delete_firstN_msgs/3,
         get_firstN_msgs/3,
	 get_all_msgs/2,
	 get_msg/2,
	 raw_msg/2,
	 to_atom/1, 
	 to_binary/1, 
	 from_json/1
         ]).


-record(message_record, 
                {
                id,
                time,
                username,
                message
                }
).


system_message(Tab,  Msg)->
       Ref = erlang:make_ref(),
       ets:insert( Tab,  #message_record{ 
                                id = Ref,
                                time = now(),
                                username = <<"system">>,
                                message = Msg
                          }),
       Ref                   
.
create_store(Tab)->
	create_store(Tab, undefined).

create_store(Tab, {ok, FileName})->
        ets:file2tab(FileName)
;
create_store(Tab, undefined)->
        ets:new(Tab, [public, named_table, ordered_set, {keypos, 2} ])
.

last(Tab)->
        ets:last(Tab)
.

     %[{<<"time">>, TimeSecs}, {<<"username">>,Username}, {<<"message">>,Msg}]
     %

from_json(KeyL)->
   Time = propslist:get_value(KeyL, <<"time">>), 
   Username = propslist:get_value(KeyL, <<"username">>),
   Message = propslist:get_value(KeyL, <<"messsage">>),
   % TimeSecs = (Mega * 1000000) + Sec,
   Mega = trunc(Time/1000000),
   Secs = Time rem 1000000,
   #message_record{id=erlang:make_ref(), time={Mega, Secs, 0 }, username=Username, message=Message}.



get_all_msgs(Tab, ProcessFun)->
 List = ets:tab2list(Tab),
 lists:map(fun(Msg)-> ProcessFun( Msg#message_record.id,
				  Msg#message_record.time,
				  Msg#message_record.username,
				  Msg#message_record.message
				) end, List )
.

get_last_count(Tab, From, Count, Fun)->
           RevesedList  =
           case 
                ets:lookup(Tab, From) of
             [Msg] ->
                NewFrom = ets:prev(Tab, From),
                get_from_reverse3(Tab, NewFrom,  Count - 1, [ Msg ]);
              [] -> []
           end,
           lists:foldl(fun(Msg, Accum)-> 
                           NewItem =
                              Fun(Msg#message_record.id,
                                Msg#message_record.time,
                                Msg#message_record.username,
                                Msg#message_record.message
                              ),
                           [NewItem | Accum]                    
                       end, [],
                       RevesedList
                       )
        
.

get_from_reverse(_Tab, _Index, _Index, _Fun)->
        []
;
get_from_reverse(Tab, From, Index, Fun)->
        RevesedList  =
           case 
                ets:lookup(Tab, From) of
             [Msg] ->
                NewFrom = ets:prev(Tab, From),
                get_from_reverse2(Tab, NewFrom,  Index, [ Msg ]);
                
             [] -> []
           end,
           lists:foldl(fun(Msg, Accum)-> 
                           NewItem =
                              Fun(Msg#message_record.id,
                                Msg#message_record.time,
                                Msg#message_record.username,
                                Msg#message_record.message
                              ),
                           [NewItem | Accum]                    
                       end, [],
                       RevesedList
                       )
           
.


delete_firstN_msgs(Tab, Count, Fun)->
        Key  = ets:first(Tab),
        delete_firstN_msgs(Tab, Key,  1, Count, Fun)
.


delete_firstN_msgs(_Tab, '$end_of_table',  _Index, _Count, _Fun)->
        true
;
delete_firstN_msgs(_Tab, _Key,  _Count, _Count, _Fun)->
        true
;
delete_firstN_msgs(Tab, Key,  Index, Count, Fun)->
        case ets:lookup(Tab, Key ) of
             [Msg] ->
                Fun(Msg#message_record.id,
                                Msg#message_record.time,
                                Msg#message_record.username,
                                Msg#message_record.message
                              ),
                ets:delete(Tab, Key);
             [] ->        
                ets:delete(Tab, Key)
        end,     
        NewKey  = ets:first(Tab),
        delete_firstN_msgs(Tab, NewKey, Index + 1, Count, Fun)
.

get_firstN_msgs(Tab, Count, Fun)->
          ets:safe_fixtable(Tab,true),
          From = ets:first(Tab),
          RevesedList  =
           case 
                ets:lookup(Tab, From) of
             [Msg] ->
                NewFrom = ets:next(Tab, From),
                get_firstN_msgs(Tab, NewFrom,  1, Count,  [ Msg ]);
                
             [] -> []
           end,
           ets:safe_fixtable(Tab, false),
           lists:foldl(fun(Msg, Accum)-> 
                           NewItem =
                              Fun(Msg#message_record.id,
                                Msg#message_record.time,
                                Msg#message_record.username,
                                Msg#message_record.message
                              ),
                           [NewItem | Accum]                    
                       end, [],
                       RevesedList
                       )

.

get_firstN_msgs(_Tab, '$end_of_table',  _Index, _Count, Accum)->
        Accum
;
get_firstN_msgs(_Tab, _NewFrom,  _Count, _Count, Accum)->
        Accum
;
get_firstN_msgs(Tab, From,  Index, Count, Accum)->
         case 
                ets:lookup(Tab, From) of
             [Msg] ->
                NewFrom = ets:next(Tab, From),
                get_firstN_msgs(Tab, NewFrom,  Index + 1, Count,  [ Msg | Accum]);
             [] -> Accum
           end.

get_from_reverse3(_Tab, _Index, 0, Accum)->
        Accum
;
get_from_reverse3(Tab, From, Index, Accum)->
        case 
                ets:lookup(Tab, From) of
        [ Msg ] ->         
                NewForm = ets:prev(Tab, From),
                get_from_reverse3(Tab, NewForm, Index - 1, [Msg|Accum]);
        [] ->
                Accum
        end
.



get_from_reverse2(Tab, Index, Index, Accum)->
 
                Accum
;
get_from_reverse2(Tab, From, Index, Accum)->
        case 
                ets:lookup(Tab, From) of
        [ Msg ] ->         
                NewForm = ets:prev(Tab, From),
                get_from_reverse2(Tab, NewForm, Index, [Msg|Accum]);
        [] ->
                Accum
        end
.


-spec put_new_message(atom(), tuple()  )-> true.

put_new_message(Tab, { Username, MessBin } )->
       Ref = erlang:make_ref(),
       ets:insert( Tab,  #message_record{ 
                                id = Ref,
                                time = now(),
                                username = Username,
                                message = MessBin
                          }),
       Ref                   
.
get_msg(Tab, Id)->
    case ets:lookup(Tab , Id)  of 
	[Msg] -> { Msg#message_record.time, Msg#message_record.username,  Msg#message_record.message};
        [] -> undefined
    end.
		  




raw_msg(Tab, Msg )->
       Ref = erlang:make_ref(),
       ets:insert( Tab, Msg#message_record{id=Ref}),
       Ref
.



to_binary(E) when is_integer(E)->
	to_binary(integer_to_list(E));
to_binary(E) when is_atom(E)->
	to_binary(atom_to_list(E));
to_binary(E) when is_list(E)->
	list_to_binary(E)

to_atom(E) when is_integer(E)->
  E1 = integer_to_list(E),
  to_atom(E);
to_atom(E)   when is_binary(E)->
  E1 = binary:bin_to_list(E),
  to_atom(E1);
to_atom(E) when is_list(E)->
  list_to_atom(E).

