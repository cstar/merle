-module(merle_app).
-behaviour(application).

-export([
	 start/2,
	 stop/1
	 ]).

start(_Type, _StartArgs)->
    case get(file, "KETAMA_FILE") of
        error ->
            {error, "please set the file application variable or the KETAMA_FILE environment variable"};
        KetamaServers ->
            merle_sup:start_link(KetamaServers)
    end.
    
stop(_State)->
    ok.
       
get(Atom, Env)->
    case application:get_env(merle, Atom) of
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