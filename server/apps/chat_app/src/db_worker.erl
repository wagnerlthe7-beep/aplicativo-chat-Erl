-module(db_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {conn :: epgsql:connection() | undefined}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    
    case epgsql:connect(Host, Username, Password, [{database, Database}, {port, Port}]) of
        {ok, Conn} ->
            {ok, #state{conn = Conn}};
        {error, Reason} ->
            {stop, {connection_failed, Reason}}
    end.

handle_call(get_connection, _From, State = #state{conn = Conn}) ->
    {reply, {ok, Conn}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({return_connection, _Conn}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    epgsql:close(Conn),
    ok.