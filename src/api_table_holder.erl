-module(api_table_holder).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, status/0, start_archive/0, flush_chat/0, flush_chat/1, archive/3, new_message/1, 
	 clear_online/0, backup_messages/0, create_store/2, create_store/1, save_chat/2,restore_chat/1 ]).

-include("erws_console.hrl").

-record(monitor,{
                  messages,
                  users, 
		  timer_back,
		  mysql_pid
                }).


           
start_link() ->
          gen_server:start_link({local, ?MODULE},?MODULE, [],[]).

init([]) ->

        Back = application:get_env(erws, messages_file),
        BackInterval = application:get_env(erws, backup_messages_interval, 3601000),
        Ets = chat_api:create_store(?MESSAGES, Back),
        EtsSess = ets:new(?SESSIONS, [public, named_table, set, {keypos,2} ] ),
        EtsSess1 = ets:new(?CHATS, [public, named_table, set ] ),
	ets:insert(?CHATS, {"", [], Ets}), %% insert default store for main chat

	timer:apply_after(?INIT_APPLY_TIMEOUT, ?MODULE,
                          start_archive, []),
	
        timer:apply_interval(?INTERVAL_CLEAR, ?MODULE,
                             clear_online, []),
        
        Timer = timer:apply_interval(BackInterval, ?MODULE,
                             backup_messages, []),
            			 
	{ok, #monitor{
                        messages = Ets ,
                        users = EtsSess, 
			timer_back = Timer
                           
        }}.

restore_chat(Ref)->
    Result = gen_server:call(?MODULE, {restore_chat, Ref}),
    case Result of
         {ok, _Cols, [[Json]]} ->
              List  = erws_api:json_decode(Json),
              U1 = proplists:get_value(<<"user1">>, List),
              U2 = proplists:get_value(<<"user2">>, List),
              Ets = proplists:get_value(<<"ets">>, List),
              Ref = proplists:get_value(<<"ref">>, List),
              Messages = proplists:get_value(<<"messages">>, List),
              EtsA = chat_api:to_atom(Ets),
              api_table_holder:create_store(EtsA, Messages),
              case   proplists:get_value(<<"user3">>, List, undefined) of
	             undefined ->    ets:insert(?CHATS, {Ref, [U1, U2], EtsA});
	             U3 ->    ets:insert(?CHATS, {Ref, [U1, U2, U3], EtsA})
              end, true;
%% insert default store for main chat
      _ -> false
    end. 

backup_messages()->
    gen_server:cast(?MODULE, backup)	
.



clear_online()->
    lists:foreach(fun(E) -> 
	              Pid =  element(2, E), 
		      case is_process_alive(Pid) of 
			   false->  
	                       ets:delete(?SESSIONS, Pid); 
			   _-> nothing 
		      end      
		  end, ets:tab2list(?SESSIONS)).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





handle_call({restore_chat, Ref},_From, MyState)->
   Pid = MyState#monitor.mysql_pid, 
   Result = mysql:query(Pid, <<"SELECT history FROM  chats WHERE ref=?">>, [Ref]), 
   {reply, Result, MyState};  
handle_call(status,_From ,State) ->
    ?LOG_DEBUG("get msg call ~p ~n", [status]),
    {reply, {   ets:tab2list(State#monitor.messages), ets:tab2list(State#monitor.users) } ,State};
handle_call(Info,_From ,State) ->
    ?LOG_DEBUG("get msg call ~p ~n", [Info]),
    {reply, {   ets:tab2list(State#monitor.messages), ets:tab2list(State#monitor.users) } , State}.

 
start_archive()->
      gen_server:cast(?MODULE, archive_mysql_start).  

flush_chat()->
      gen_server:cast(?MODULE, {flush_chat, ?DEFAULT_FLUSH_SIZE})   
.

save_chat(Ref, History)->
    gen_server:cast(?MODULE, {save_chat, Ref, History})   
.	

flush_chat(Count)->
    gen_server:cast(?MODULE, {flush_chat, Count})      
.       
new_message(Msg)->
    gen_server:cast(?MODULE, {new_msg, Msg}).      

stop() ->
    gen_server:cast(?MODULE, stop).

create_store(Atom)->
	create_store(Atom,[]).

create_store(Atom, List) ->
    gen_server:cast(?MODULE, {create_store, Atom, List}).
	
 
process_to_archive(_Msid,  _Msgtime, Msgusername,  Msgmessage  )->
        emysql:execute(?MYSQL_POOL, stmt_arhive,[Msgmessage, Msgusername])
.

handle_cast(backup, MyState)->
    L = ets:tab2list(?CHATS),
    lists:foreach(fun({Chat, _L,  _Ets}) -> spawn_link(erws_api, backup_chat, [Chat])  end, L),
    {noreply, MyState};
handle_cast(stop, MyState) ->
     io:format("somebody wants me dying\n", []),
    {stop, this_painful_world ,MyState};  
handle_cast({new_msg, Msg}, MyState) ->
    chat_api:raw_msg(MyState#monitor.messages, Msg),     
    {noreply, MyState};   
handle_cast({create_store, Atom, L}, MyState) ->
    chat_api:create_store(Atom),
     %[{<<"time">>, TimeSecs}, {<<"username">>,Username}, {<<"message">>,Msg}]
    Objs = lists:map(fun(E) -> chat_api:from_json(E) end, L),
    ets:insert(Atom, Objs),
    {noreply, MyState};  
handle_cast({save_chat, Ref, History}, MyState)->
   Pid = MyState#monitor.mysql_pid, 	
   mysql:query(Pid, <<"INSERT INTO chats(ref, history) VALUES (?,?)
		       ON DUPLICATE KEY UPDATE  history=? ">>, [Ref, History, History]), 
   {noreply, MyState};  
handle_cast({flush_chat, Count }, MyState) ->
    chat_api:delete_firstN_msgs(MyState#monitor.messages, Count, fun process_to_archive/4),     
    {noreply, MyState};   
handle_cast( archive_mysql_start, MyState) ->
    ?LOG_DEBUG("start archiving ~p ~n", [MyState]),

    {ok, User} = application:get_env(erws, mysql_user),
    {ok, Host} = application:get_env(erws, mysql_host),
    {ok, Pwd} = application:get_env(erws,
                                      mysql_pwd),
    {ok, Base} = application:get_env(erws,
                                      database), 
    {ok, MaxSize } = application:get_env(erws, ets_max_size),
    {ok, Size } = application:get_env(erws, archive_size),
    {ok, Interval } = application:get_env(erws, archive_interval),
    
  %  timer:apply_interval(Interval, api_table_holder, archive, [ MyState#monitor.messages, MaxSize, Size] ),
   
    {ok, Pid} = mysql:start_link([{host, Host}, {user,  User},
                                  {password, Pwd}, {database, Base}
                                  ]),

    ?CONSOLE_LOG("connect to mysql ~p", [Pid]),
 
    {noreply, MyState#monitor{mysql_pid=Pid}}.
    
archive(Tab, MaxSize, Size)->
        case MaxSize < ets:info(Tab, size) of
                true->
                       ?MODULE:flush_chat(Size);
                false-> do_nothing
        end
.

handle_info(_,  State)->
   
    {noreply,  State}.

terminate(_Reason, _State) ->
   terminated.

status() ->
        gen_server:call(?MODULE, status)
    .




