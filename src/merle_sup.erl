-module(merle_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(KetamaFile) ->
    supervisor:start_link({local, ?MODULE},?MODULE, [KetamaFile]).

init([KetamaFile]) ->
    {ok, {{one_for_one, 1, 60},
          [{merle, {merle, start_link, [KetamaFile]},
            temporary, brutal_kill, worker, [merle]},
            {ketama, {ketama, start_link, [KetamaFile]},
            temporary, brutal_kill, worker, [ketama]}]}}.