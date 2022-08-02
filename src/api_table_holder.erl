-module(api_table_holder).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, status/0, start_archive/0, flush_chat/0, flush_chat/1, archive/3, new_message/1, 
	 clear_online/0, backup_messages/1, create_store/1 ]).

-include("erws_console.hrl").

-record(monitor,{
                  messages,
                  users, 
		  timer_back
                }).


           
start_link() ->
          gen_server:start_link({local, ?MODULE},?MODULE, [],[]).

init([]) ->

        Back = application:get_env(erws, messages_file),
        BackInterval = application:get_env(erws, backup_messages_interval, 3601000),
        Ets = chat_api:create_store(?MESSAGES, Back),
        EtsSess = ets:new(?SESSIONS, [public, named_table, set, {keypos,2} ] ),
        EtsSess1 = ets:new(?CHATS, [public, named_table, set ] ),
	ets:insert(?CHATS, {"", undefined, undefined, Ets}), %% insert default store for main chat

	%timer:apply_after(?INIT_APPLY_TIMEOUT, ?MODULE,
        %                  start_archive, []),
	%
        timer:apply_interval(?INTERVAL_CLEAR, ?MODULE,
                             clear_online, []),
        
	case Back of
           undefined->
	    {ok, #monitor{
                        messages = Ets ,
                        users = EtsSess
                           
            } };
          {ok, FileName}->
             Timer = timer:apply_interval(BackInterval, ?MODULE,
                             backup_messages, [FileName]),
            			 
	    {ok, #monitor{
                        messages = Ets ,
                        users = EtsSess, 
			timer_back = Timer
                           
            } }
        end		 
	.

backup_messages(FileName)->
     ets:tab2file(?MESSAGES, FileName)
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

flush_chat(Count)->
    gen_server:cast(?MODULE, {flush_chat, Count})      
.       
new_message(Msg)->
    gen_server:cast(?MODULE, {new_msg, Msg}).      

stop() ->
    gen_server:cast(?MODULE, stop).
 
create_store(Atom) ->
    gen_server:cast(?MODULE, {create_store, Atom}).
	
 
process_to_archive(_Msid,  _Msgtime, Msgusername,  Msgmessage  )->
        emysql:execute(?MYSQL_POOL, stmt_arhive,[Msgmessage, Msgusername])
.

handle_cast(stop, MyState) ->
     io:format("somebody wants me dying\n", []),
    {stop, this_painful_world ,MyState};  
handle_cast({new_msg, Msg}, MyState) ->
    chat_api:raw_msg(MyState#monitor.messages, Msg),     
    {noreply, MyState};   
handle_cast({create_store, Atom}, MyState) ->
    chat_api:create_store(Atom), 
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
    
 
 %   emysql:add_pool(?MYSQL_POOL, [{size,4},
%                     {user, User},
 %                    {password, Pwd},
 %                    {host, Host},
 %                    {database, Base},
 %                    {encoding, utf8}]),
%% TODO change NOW() to the value of ets table                     
   % emysql:prepare(stmt_arhive, 
   %              <<"INSERT INTO main_chathistory(msg, user, pub_date, status) VALUES(?,?, NOW(),\"processed\")">>),
    {noreply, MyState}.
    
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




