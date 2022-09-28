-module(erws_api).

-include("erws_console.hrl").


% Behaviour cowboy_http_handler
-export([init/3, hexstring/1, generate_key/2, backup_chat/1, json_decode/1, json_encode/1 ]).




% Called to know how to dispatch a new connection.
init({tcp,http}, Req, Opts) ->
    ?CONSOLE_LOG("Request api: ~p ~n", [ {Req,  Opts} ]),
    Resp = handle(Req),
    % we're not interested in serving any other content.
    {ok, Resp, Opts}
.
    

headers_text_plain() ->
	#{<<"access-control-allow-origin">> => <<"*">>, <<"Content-Type">> => <<"text/plain">>}.
        
headers_text_html() ->
	#{<<"access-control-allow-origin">> =>  <<"*">>,  <<"Content-Type">> => <<"text/html">>}.      

headers_json_plain() ->
	[{<<"access-control-allow-origin">>, <<"*">>},  {<<"Content-Type">>, <<"application/json">>}].
        
        
% Should never get here.
handle(Req0) ->
      ?CONSOLE_LOG("====================================~nrequest: ~p ~n", [Req0]),
      {Path, Req }  = cowboy_req:path_info(Req0),
      ?CONSOLE_LOG("====================================~npath: ~p ~n", [Path]),
      {ok, Body, Req2} = cowboy_req:body_qs(Req),
%	Echo = proplists:get_value(<<"echo">>, PostVals),

      case process(Path, Body, Req2) of
	  {json, Json, ResReqLast }->
		?CONSOLE_LOG("got request result: ~p~n", [Json]),
		JsonReq = cowboy_req:reply(200, headers_json_plain(), json_encode(Json), ResReqLast),
		JsonReq;
          {raw_answer, {Code, Binary, Headers }, ResReqLast } ->
		RawReq = cowboy_req:reply(Code, Headers, Binary, ResReqLast),
		RawReq
      end.      


false_response(Req)->
   {raw_answer, {500, <<"{\"status\":\"false\"}">>, headers_json_plain() },  Req}.
 
true_response(Req)->
   {raw_answer, {200, <<"{\"status\":\"true\"}">>, headers_json_plain() },  Req}.

process([?ADMIN_KEY, <<"chat">>, <<"create">>, Key], Body, Req) ->
     ?CONSOLE_LOG("create chat for  ~p ~n",[Req]),
     
     {User1, Req_1} = cowboy_req:qs_val(<<"user1">>, Req, undefined),
     {User2, Req_2} = cowboy_req:qs_val(<<"user2">>, Req_1, undefined),
     KeyA  = chat_api:to_atom(Key),
     %% TODO check the duplication of p2p chats
     api_table_holder:create_store(KeyA),
     ets:insert(?CHATS, {Key, [User1, User2], KeyA}), 
     true_response(Req_2)
;
process([?ADMIN_KEY,<<"unban">>,Username],  _Body, Req)->
     ?CONSOLE_LOG("undefined ban from ~p ~n",[Req]),
     mcd:delete(myMcd, <<"cryptonbanned_", Username/binary>>),
     true_response(Req);
process([?ADMIN_KEY, <<"ban">>,Username],  _Body, Req)->
     ?CONSOLE_LOG("undefined ban from ~p ~n",[Req]),
     mcd:set(myMcd, <<"cryptonbanned_", Username/binary>>, <<1>>,60000),
     lists:foreach(fun({_, Ref, _, Login, _Txt})-> 
          case Login of 
             Username -> ets:delete(ets_sessions_holder, Ref); 
             _-> do_nothing 
          end  
     end,  ets:tab2list(ets_sessions_holder)),
     true_response(Req);
process([?ADMIN_KEY,<<"add_user">>, Username, Chat],  _Body, Req)->
     %TODO move api_tableholder	
     case ets:lookup(?CHATS, Chat) of 
         []-> false_response(Req);	     
	 [{Chat, L, Ets}]->
		ets:insert(?CHATS, {Chat, [Username|L], Ets}),
     		true_response(Req) 
     end;
process([?ADMIN_KEY,<<"remove_user">>, Username, Chat],  Body, Req)->
     %TODO move api_tableholder	
     case ets:lookup(?CHATS, Chat) of 
         []-> false_response(Req);	     
	 [{Chat, L, Ets}]->
		      
		ets:insert(?CHATS, {Chat, lists:delete(Username, L ), Ets}),
     		true_response(Req) 
     end;
process([?ADMIN_KEY,<<"post">>, Username],  Body, Req)->
     Echo = proplists:get_value(<<"msg">>, Body),      
     chat_api:put_new_message(?MESSAGES, {Username, Echo}),
     ?CONSOLE_LOG("request  post from ~p ~n",[Req]),
     true_response(Req);
process([?ADMIN_KEY,<<"post">>, Username, Chat],  Body, Req)->
     Echo = proplists:get_value(<<"msg">>, Body),    
     ?CONSOLE_LOG("request  post from ~p to ~p msg ~p ~n",[Req, Chat, Echo]),
     case ets:lookup(?CHATS, Chat)  of 
	 [{Chat, _L,  Ets}]->
		chat_api:put_new_message(Ets, {Username, Echo}),
                true_response(Req);
	 []-> false_response(Req)
     end;

process([?ADMIN_KEY, <<"save">>,  Chat],  _Body, Req)->
     ?CONSOLE_LOG("request  get messages from ~p to ~p ~n",[Req, Chat ]),
     case backup_chat(Chat)  of 
	 undefined-> false_response(Req);
	 Json ->
              {json, Json, Req }

     end
;
process([?ADMIN_KEY, <<"messages">>,  Chat],  _Body, Req)->
     ?CONSOLE_LOG("request  get messages from ~p to ~p ~n",[Req, Chat ]),
     case ets:lookup(?CHATS, Chat)  of 
	 [{Chat, _L, Ets}]->
		     From  = chat_api:last(Ets),
                     List = chat_api:get_last_count(Ets, From, 65000, fun process_chat_msg/4),   
		     ?CONSOLE_LOG("chat list: ~p ~n~n", [List]),
                     Json = json_encode([{<<"status">>,true},
				         {<<"new_messages">>, List } ]),
								                 
                     {json, Json, Req };

	 []->  false_response(Req)
     end;

process(_, _Body, Req)->
     ?CONSOLE_LOG("undefined request from ~p ~n",[Req]),
     false_response(Req).

backup_chat(Cht)->
     case ets:lookup(?CHATS, Cht)  of 
	 [{Cht, LU,  Ets}]->
              List = chat_api:get_all_msgs(Ets, fun process_chat_msg/4),   
	      JsonL = [
				  {<<"ref">>, Cht},
				  {<<"ets">>, chat_api:to_binary(Ets) },
				  {<<"messages">>, List }
		      ],
	      ResJson = lists:foldl(fun(E, Sum)->      
			                    U = lists:nth(E, LU),
	                                    IB = integer_to_binary(E),
			                    [{<<"user", IB/binary>>, chat_api:to_binary(U) }| Sum]
			            end, 
			            JsonL,
			            lists:seq(1, length(LU))),
              Json = json_encode(ResJson),
              api_table_holder:save_chat(Cht, Json),
	      Json;
	 []-> undefined
     end
.


process_chat_msg(D1,D2,D3,D4)->
	erws_handler:process_chat_msg(D1,D2,D3,D4).




     
generate_key(Salt, Body)->
        hexstring( crypto:hash(sha256, <<Salt/binary, Body/binary >>)  ) 
.

json_decode(Json)->
        jsx:decode(Json).

json_encode(Doc)->
        jsx:encode(Doc).


-spec hexstring( binary() ) -> list().
hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).




