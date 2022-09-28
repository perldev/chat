-module(erws_sup).  
-behaviour(supervisor).  
-export([start_link/0]).  
-export([init/1]).  
-include("erws_console.hrl").

start_link() ->  
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).  
      
init([]) -> 
        Api_table_holder ={
                "api_table_holder",
             {api_table_holder, start_link, [] },
             permanent, 5000, worker , [ api_table_holder]   
        
        },
        Mcd = {local_memcache,
                {mcd, start_link, [?LOCAL_CACHE, ["127.0.0.1", 11211]]},
               permanent, 10000, worker, [mcd] },
                
                
        {ok, { {one_for_one, 5, 10}, [ Mcd ,Api_table_holder ] } }.  
