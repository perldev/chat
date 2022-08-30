%-define('CONSOLE_LOG'(Str, Params), true ).
%-define('LOG_DEBUG'(Str, Params), true ).
-define('CONSOLE_LOG'(Str, Params), lager:info(Str, Params) ).
-define('LOG_DEBUG'(Str, Params), lager:debug(Str, Params) ).
-define(INIT_APPLY_TIMEOUT,1000).
-define(INTERVAL_CLEAR, 30000).
-define(HOST,"http://127.0.0.1:8098").
-define(RANDOM_CHOICE, 10).
-define(SESSIONS, ets_sessions).
-define(MYSQL_POOL, mysql_pool).
-define(DEFAULT_FLUSH_SIZE, 1000).
-define(MESSAGES, ets_sessions_holder).
-define(UNDEF, undefined).
-define(SESSION_SALT_CODE, <<"aasa_salts">> ).
-define(SESSION_SALT, <<"tesC_aasa_salts">> ).
-define(ADMIN_KEY, <<"tass_token">> ).
-define(KEY_PREFIX, "crypton" ).
-define(LOCAL_CACHE, 'myMcd').
-define(CHATS, chats).



-record(message_record,
	                {
			  id,
			  time,
			  username,
			  message
		        }
       ).



-record(
        chat_state,{
	       pid,
               start,
               last_post = 0,
               index = 0,
               username = "",
               ip,
               last_msg,
	       opts,
	       chat=undefined
        }

).




