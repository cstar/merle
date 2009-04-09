%%%-------------------------------------------------------------------
%%% File    : ketama.erl
%%% Author  : Richard Jones <rj@last.fm>
%%% Description : Port driver for libketama hasing
%%%-------------------------------------------------------------------
-module(ketama).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, start_link/2, getserver/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {port, filename, last_modified, exe}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    start_link("/web/site/GLOBAL/ketama.servers").

start_link(ServersFile) ->
    Bin = code:priv_dir(merle)++"/"++"ketama_erlang_driver",
    start_link(ServersFile, Bin).

start_link(ServersFile, BinPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ServersFile, BinPath], []).

getserver(Key) ->
    gen_server:call(?MODULE, {getserver, Key}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server 
%% ServersFile: ketama.servers list
%% BinPath: path to ketama_erlang_driver binary
%%--------------------------------------------------------------------
init([ServersFile, BinPath]) ->
    Exe = BinPath ++ " " ++ ServersFile,
    Port = open_port({spawn, Exe}, [binary, {packet, 1}, use_stdio]),
    LastMod = filelib:last_modified(ServersFile),
    {ok, #state{port=Port,filename=ServersFile,last_modified=LastMod, exe=Exe}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({getserver, Key}, _From, #state{port=Port, filename=ServersFile,last_modified=LastMod, exe=Exe}=State) ->
    ModDate = filelib:last_modified(ServersFile),
    NewState = case  (ModDate /= LastMod) of
        true ->
            error_logger:info_msg("Ketama : Reloading driver (server file changed changed)", []),
            port_close(Port),
            NewPort = open_port({spawn, Exe}, [binary, {packet, 1}, use_stdio]),
            #state{port=NewPort,filename=ServersFile,last_modified=ModDate, exe=Exe};
        false ->
            State
    end,
    Port2 = NewState#state.port,
    Port2 ! {self(), {command, Key}},
    receive
        {Port2, {data, Data}} ->
            {reply, Data, NewState}
        after 1000 -> % if it takes this long, you have serious issues.
            error_logger:info_msg("Timeout on ~p", [Key]),
            {stop, ketama_port_timeout, NewState}
    end;

handle_call(Msg, From, State)->
    error_logger:info_msg("Received call ~p from ~p", [Msg, From]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    error_logger:info_msg("Received cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State};

handle_info(Msg, State)->
    error_logger:info_msg("Received info ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate({port_terminated, _Reason}, _State) ->
    ok;

terminate(_Reason, #state{port = Port} = _State) ->
    port_close(Port).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

