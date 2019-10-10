-module(erws_handler).

-include("erws_console.hrl").

% Behaviour cowboy_http_handler
-export([ terminate/2, init/2]).
% Behaviour cowboy_http_websocket_handler
%-behaviour(cowboy_websocket_handler).

-export([websocket_init/1]).

-export([websocket_handle/2]).

-export([websocket_info/2]).

-export([websocket_terminate/2]).


init(Req, Opts) ->
        { IP, _Port } = cowboy_req:peer(Req),
        ?CONSOLE_LOG("~n new session  ~n", []),
        State = #chat_state{ip = IP, pid=self()  ,  start = now(), username = "", opts=Opts},
        ets:insert(?SESSIONS, State),
	{cowboy_websocket, Req, State}.




terminate(_Req, _State) -> ok.

% Called for every new websocket connection.
websocket_init(State) ->
    ?CONSOLE_LOG("~nNew client ~p", [State]),
    {ok,  State}.

% Called when a text message arrives.
websocket_handle({text, Msg},  State) ->
    ?CONSOLE_LOG("~p Received:  ~n ~p~n~n",
		 [{?MODULE, ?LINE}, State]),
    Message = json_decode(Msg),
    ?CONSOLE_LOG(" Req: ~p ~n", [Message]),
    {Res, NewState} = process_req(State, Message),
    ?CONSOLE_LOG("~p send back: ~p ~n",
		 [{?MODULE, ?LINE}, {NewState, Res}]),
    {reply, {text, Res},  NewState};
% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Any,  State) ->
    ?CONSOLE_LOG("unexpected: ~p ~n ~p~n~n", [Any, State]),
    {ok,  State}.

% Other messages from the system are handled here.
% Other messages from the system are handled here.
websocket_info({new_message, Msg}, Req, State) ->
     ?CONSOLE_LOG("info: ~p ~n ~p~n~n", [Req, State]),
     {reply, {text, }, State}.
websocket_info(_Info,  State) ->
    ?CONSOLE_LOG("info: ~p ~n ~p~n~n", [State]),
    {ok,  State}.

websocket_terminate(Reason, State) ->
    ?CONSOLE_LOG("terminate: ~p ,~n ~p, ~n ~p~n~n",
		 [Reason,  State]),
    ets:delete(?SESSIONS, State#chat_state.start),		 
    ok.
    
%     Doc4 =   [ {[{<<"bing">>,1},{<<"test">>,2}]}, 2.3, true] .
% [{[{<<"bing">>,1},{<<"test">>,2}]},2.3,true]
% (shellchat@localhost.localdomain)16> jiffy:encode( Doc4).                                      
% <<"[{\"bing\":1,\"test\":2},2.3,true]">>
% 

send_them_all(State, Message)
        ets:foldl(fun(Elem, Acc) ->  Elem#chat_state.pid ! {new_message, Message } end, [], ?SESSIONS).

process_req(State  = #chat_state{ index = 0},
                [ {<<"ping">>, _}] )->
            From  = chat_api:last(?MESSAGES),
            List = chat_api:get_last_count(?MESSAGES, From, 100, fun process_chat_msg/4),    
            ?CONSOLE_LOG("chat list: ~p ~n~n", [List]),
            Json = json_encode([{<<"status">>,true},
                                {<<"new_messages">>, List } ]  ),
            { Json, State#chat_state{ index = From } }
; 
process_req(State  = #chat_state{ index = Index},
                [ {<<"ping">>, _}] )->
            From  = chat_api:last(?MESSAGES),
            ?CONSOLE_LOG("ping from  ~p  to ~p ",
                 [From, Index]),
            List = chat_api:get_from_reverse(?MESSAGES, From, Index, fun process_chat_msg/4),    
            Json = json_encode([{<<"status">>,true},
                                {<<"new_messages">>, List } ]  ),
            { Json, State#chat_state{ index = From } }
;         
process_req(State  = #chat_state{username = "", index = Index },
                {[ {<<"new_message">>, OldMsg},{<<"session">>, null} ]} )->
                { <<"{status:false}">>, State };
process_req(State  = #chat_state{username = "", index = Index },
                [ {<<"new_message">>, OldMsg},{<<"session">>, Session} ] )->
                Key = <<"cryptonchat_", Session/binary >>,
                ?CONSOLE_LOG("info: ~p ~n ~p key  ~p ~n~n", [Session, State, Key]),

                case mcd:get(myMcd, Key) of
                               {error, notfound} ->
                                         {<<"{\"status\":false,\"desc\":\"auth_required\"}">>, State};
                               %HACK
                               {ok, Username} ->
%                                       <<"bogdan\np1\n.">>
%                                        <<V2:8,B2/binary>> = Username,<<128,2,88,0,0,0,0,46>>
                                       ?CONSOLE_LOG("got from session: ~p ~n~n", [pickle:pickle_to_term(Username)]),
                          
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
                                                From  = chat_api:put_new_message(?MESSAGES, {RealUserName, Msg}),
                                                List  =  chat_api:get_from_reverse(?MESSAGES, From, Index,
                                                                                   fun process_chat_msg/4),

                                                Json  = json_encode([{<<"status">>,true},
                                                                     {<<"new_messages">>, List } ]  ),
                                                { Json,  NewState#chat_state{index = From} };
                                             false->
                                                { <<"{status:false}">>,  NewState }
                                                
                                       end 
                 end              

;    
process_req(State  = #chat_state{last_post = Time, index = Index, 
                     username = Username},
                [ {<<"new_message">>, OldMsg},{<<"session">>, _Session} ] )->
       Msg = filter_message(OldMsg),
       case filters(State#chat_state{last_msg = Msg }) of
           true ->

                From  = chat_api:put_new_message(?MESSAGES, {Username, Msg}),
                List = chat_api:get_from_reverse(?MESSAGES, From, Index, fun process_chat_msg/4),
                Json = json_encode([{<<"status">>,true},
                                    {<<"new_messages">>, List } ] ),                     
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
                        Res = timer:now_diff(Now, Time)> 15000000,
                        ?CONSOLE_LOG("filter: is it too often ~p ~n ", [Res]),
                        Res
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

