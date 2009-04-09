-module(merle_app).
-behaviour(application).

-export([
	 start/2,
	 stop/1
	 ]).

start(_Type, _StartArgs)->
    KetamaServers = get(file, "KETAMA_FILE"),
    merle_sup:start_link(KetamaServers).
    
stop(_State)->
    ok.
       
get(Atom, Env)->
    case application:get_env(Atom) of
     {ok, Value} ->
         Value;
     undefined ->
         case os:getenv(Env) of
     	false ->
     	    error;
     	Value ->
     	    Value
         end
    end.