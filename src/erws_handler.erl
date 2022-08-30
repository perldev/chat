-module(erws_handler).

-include("erws_console.hrl").

% Behaviour cowboy_http_handler
-export([ terminate/2, init/3]).
% Behaviour cowboy_http_websocket_handler
-behaviour(cowboy_websocket_handler).

-export([websocket_init/3]).

-export([websocket_handle/3]).

-export([websocket_info/3]).

-export([websocket_terminate/3]).
% Called to know how to dispatch a new connection.
-export([process_chat_msg/4]).
init({tcp, http}, _Req, _Opts) ->
%     % "upgrade" every request to websocket,
%         % we're not interested in serving any other conten
         {upgrade, protocol, cowboy_websocket}.
%
terminate(_Req, State) -> 
                             ?CONSOLE_LOG("termination of socket: ~p ,~n",
                                             [State]).
%

get_key_dict(SessionObj,Key, Default)->
	    case dict:find(Key, SessionObj) of
		            {ok, Value} -> Value;
		            error -> Default
            end.


load_user_session(SessionKey)->
	  case mcd:get(?LOCAL_CACHE, SessionKey) of
		      {ok, Val}->
			  	% add saving to localcache
			   	pickle:pickle_to_term(Val);
	               _ ->
		                undefined  
	  end.

django_read_token(Session)->
	    <<?KEY_PREFIX, "read_token", Session/binary>>.
 
auth_user(CookieSession)->
       ?CONSOLE_LOG(" auth for session  ~p ~n",[ CookieSession]),
       case CookieSession of 
               undefined ->
                 ?CONSOLE_LOG(" nothing found for auth for session  ~p ~n",[ CookieSession]),
                 "";
                 _ ->
                            ?CONSOLE_LOG(" start found for auth for session  ~p ~n",[ CookieSession]),
		            KeyToken =  django_read_token(CookieSession),
                            ?CONSOLE_LOG(" token  ~p ~n",[ KeyToken]),
		            SessionObj =  load_user_session(KeyToken),
		            ?CONSOLE_LOG(" load session ~p ~n",[SessionObj]),
		            case SessionObj of 
		                undefined -> "";
		                SessionObj ->
		                   case get_key_dict(SessionObj, {pickle_unicode, <<"username">> }, false) of

		                             false ->  "";
					     {pickle_unicode, Username} -> Username 
		                   end
		             end      
       end.


% Called for every new websocket connection.
websocket_init(_Any, Req, []) ->
    { IP, _Port } = cowboy_req:peer(Req),
    {Path, Req0 }  = cowboy_req:path_info(Req),
    
    ?CONSOLE_LOG("~nNew client ~p", [Req]),
     {CookieSession, Req_3} = cowboy_req:qs_val(<<"token">>, Req0, undefined), 
     UserName = auth_user( CookieSession ),
     ?CONSOLE_LOG(" username ~p ~n",[UserName]),
     ReqRes = cowboy_req:compact(Req_3),
     case Path of
	  [] ->
		 %%we use default chat for all
                 State  =#chat_state{ip = IP, pid=self(), start=now(), username = UserName, opts=[], chat=""},
                 ets:insert(?SESSIONS, State),
                 {ok,  ReqRes,  State};
	  [ Chat ]  ->
		 %%we use default chat for all
                 S = #chat_state{ip = IP, pid=self(), start=now(), username = UserName, opts=[], chat=Chat},
                 %%ok lets check the state
		 case ets:lookup(?CHATS, Chat) of 
		      []-> {stop,  S};%%close connection there is no such chat  
		      [{Chat, UserName, _User2, _Msgs}]->  
				 ets:insert(?SESSIONS, S),
				 {ok,  ReqRes, S};
		      [{Chat, _User1 ,UserName, _Msgs}]->  
				 ets:insert(?SESSIONS, S),
				 {ok,  ReqRes,  S};
		       _ ->  %it's private chat  only for User1 and User2 and system
                                 {stop, S}

                 end

        end.
% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
    ?CONSOLE_LOG("~p Received:  ~n ~p~n~n ~p",
		 [{?MODULE, ?LINE}, State, Msg]),
    Message = json_decode(Msg),
    ?CONSOLE_LOG(" Req: ~p ~n", [Message]),
    %% choose the chat
    case ets:lookup(?CHATS, State#chat_state.chat) of
	[{_Key, _,_,  Ets}] -> 
             {Res, NewState} = process_req(State, Message, Ets),
             ?CONSOLE_LOG("~p send back: ~p ~n",
		 [{?MODULE, ?LINE}, {NewState, Res}]),
             {reply, {text, Res}, Req,   NewState};
        [] ->
             {stop, State}
		     

    end;
% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Any, Req,   State) ->
    ?CONSOLE_LOG("unexpected: ~p ~n ~p~n~n", [Any, State]),
    {ok, Req,  State}.

% Other messages from the system are handled here.
websocket_info({new_message, Msg},  Req,  State) ->
     ?CONSOLE_LOG("receive message:  ~n ~p ~p ~n~n", [ State, Msg]),
     {reply, {text, Msg }, Req, State};
websocket_info(Info,  Req,  State) ->
    ?CONSOLE_LOG("info: ~p ~n ~p~n~n", [Info, State]),
    {ok, Req,   State}.


websocket_terminate(Reason,  Req, State) ->
    ?CONSOLE_LOG("terminate: ~p ,~n ~p, ~n ~p~n~n",
		 [Reason, Req, State]),
    ets:delete(?SESSIONS, State#chat_state.start),		 
    ok.
    
%     Doc4 =   [ {[{<<"bing">>,1},{<<"test">>,2}]}, 2.3, true] .
% [{[{<<"bing">>,1},{<<"test">>,2}]},2.3,true]
% (shellchat@localhost.localdomain)16> jiffy:encode( Doc4).                                      
% <<"[{\"bing\":1,\"test\":2},2.3,true]">>
% 

send_them_all({Time, Username, Msg}, Chat)->
	       ets:foldl(fun(Elem, Acc) ->  
                           case Elem#chat_state.chat of
                                Chat ->
                                    ?CONSOLE_LOG("send message with this id: ~p to ~p ~n~n", [{Username, Msg}, Elem#chat_state.pid]),
			             Elem#chat_state.pid ! {new_message,  json_encode([{<<"status">>, true}, {<<"new_messages">>, [ process_chat_msg(0, Time, Username, Msg) ] }]) };
			         _ ->
                                   ?CONSOLE_LOG("this message is not for ~p", [Elem])
			     end
	        end, [], ?SESSIONS).
	                                                                 

process_req(State  = #chat_state{ index = 0},
                [ {<<"ping">>, _}], Ets )->
            From  = chat_api:last(Ets),
            Size   = ets:info(?SESSIONS, size),
            List = chat_api:get_last_count(Ets, From, 100, fun process_chat_msg/4),    
            ?CONSOLE_LOG("chat list: ~p ~n~n", [List]),
            Json = json_encode([{<<"status">>,true},
                                {<<"subscribers">>, Size},
                                {<<"new_messages">>, List } ]  ),
            { Json, State#chat_state{ index = From } }
;






process_req(State  = #chat_state{ index = Index},
                [ {<<"ping">>, _}], Ets )->
            From  = chat_api:last(Ets),
            ?CONSOLE_LOG("ping from  ~p  to ~p ",
                 [From, Index]),
            Size    = ets:info(?SESSIONS, size),                 
            List = chat_api:get_from_reverse(Ets, From, Index, fun process_chat_msg/4),    
            Json = json_encode([{<<"status">>,true},
                                {<<"subscribers">>, Size},
                                {<<"new_messages">>, [] } ]  ),
            { Json, State#chat_state{ index = From } }
;         
process_req(State  = #chat_state{username = "", index = Index },
                {[ {<<"new_message">>, OldMsg},{<<"session">>, null} ]}, _Ets )->
                { <<"{status:false}">>, State };
process_req(State  = #chat_state{username = "", index = Index},
                [ {<<"new_message">>, OldMsg},
                  {<<"session">>, Session} ], Ets )->
                Key = <<"cryptonchat_", Session/binary >>,
                ?CONSOLE_LOG("info: ~p ~n ~p key  ~p ~n~n", [Session, State, Key]),

                case mcd:get(myMcd, Key) of
                               {error, notfound} ->
                                         {<<"{\"status\":false,\"desc\":\"auth_required\"}">>, State};
                               %HACK
                               {ok, Username} ->
%                                       <<"bogdan\np1\n.">>
%                                        <<V2:8,B2/binary>> = Username,<<128,2,88,0,0,0,0,46>>
                                       ?CONSOLE_LOG("got from session username: ~p ~n~n", [pickle:pickle_to_term(Username)]),
                          
                                       RealUserName = case pickle:pickle_to_term(Username) of
                                                             {pickle_unicode, RealUserName_} -> RealUserName_;
                                                             RealUserName_ when is_binary(RealUserName_) -> RealUserName_;
                                                              _ -> <<"unrecognized">>
                                                       end,
                                
                                       ?CONSOLE_LOG("got username: ~p ~n~n", [RealUserName]),
				       NewState =  State#chat_state{username = RealUserName,
                                                          last_post = now()
                                                          },
                                        Msg = filter_message(OldMsg),
                                        case filters(State#chat_state{last_msg = Msg, 
                                                                      last_post = {0,0,0},
                                                                      username = RealUserName }) of
                                             true ->
                                                From  = chat_api:put_new_message(Ets, {RealUserName, Msg}),
						StoredMsg  = chat_api:get_msg(Ets, From),
                                                send_them_all(StoredMsg, State#chat_state.chat),
                                                Json  = json_encode([{<<"status">>,true},{<<"new_messages">> ,[] } ]),
                                                { Json,  NewState#chat_state{index = From} };
                                             false->
                                                { <<"{status:false}">>,  NewState }
                                                
                                       end 
                 end              

;    
process_req(State  = #chat_state{last_post = Time, index = Index, 
                     username = Username},
                [ {<<"new_message">>, OldMsg},{<<"session">>, _Session} ], Ets )->
       Msg = filter_message(OldMsg),
       case filters(State#chat_state{last_msg = Msg }) of
           true ->

                From  = chat_api:put_new_message(Ets, {Username, Msg}),
	        StoredMsg  = chat_api:get_msg(Ets, From),
                send_them_all(StoredMsg, State#chat_state.chat),

                Json = json_encode([{<<"status">>,true}, 
				     {<<"new_messages">>, []}
				     ]),                     
                { Json, 
                                         State#chat_state{
                                                          index = From,
                                                          last_post = now()
                                                          } 
                };
           false ->
                { <<"{status:false}">>,  State }
      end   
.

json_decode(Json)->
        jsx:decode(Json).

json_encode(Doc)->
        jsx:encode(Doc).

%     Doc4 =   [ {[{<<"bing">>,1},{<<"test">>,2}]}, 2.3, true] .
% [{[{<<"bing">>,1},{<<"test">>,2}]},2.3,true]
% (shellchat@localhost.localdomain)16> jiffy:encode( Doc4).                                      
% <<"[{\"bing\":1,\"test\":2},2.3,true]">>
% 
process_chat_msg(Id, Time, Username, Msg)-> 
     {Mega, Sec, _} = Time,
     TimeSecs = (Mega * 1000000) + Sec,
     [{<<"time">>, TimeSecs}, {<<"username">>,Username}, {<<"message">>,Msg}]
.

filters(State)->
        Username = State#chat_state.username,
        Time = State#chat_state.last_post,
        Now = now(),
        case mcd:get(myMcd, <<"cryptonbanned_", Username/binary >>) of
                {ok, _ }-> 
                        ?CONSOLE_LOG("filter: user is banned ~n ~p~n~n", [Username]),
                        false;
                {error, notfound} -> 
			true 
        end
.

filter_message(Msg)->
         binary:replace(Msg,
                [<<"[">>,
                <<"]">>, 
                <<"'">>,
                <<"|">>,
                <<">">>,
                <<"<">>,
                <<"/">>,<<"\"">>,
                <<"\\">>,
                <<"@">>,
                <<"#">>,
                <<"$">>,
                <<"%">>,
                <<"^">>,
                <<"&">>,
                <<"*">>],
                <<"">>,
                [global])
.

