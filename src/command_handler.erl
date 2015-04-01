-module(command_handler).
-behaviour(gen_event).

-export([subscribe/0]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
-record(add_player, {name}).

subscribe() ->
    command_bus:add_handler(?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_event(#add_player{name=Name} = _Command , State) ->
    io:format("Received command add_player with name: ~p~n", [Name]),
    case players:find(Name) of
        {error, not_found} ->
            io:format("Start new player~n", []);
        {ok, _Pid} ->
            io:format("There is already a player with name: ~p~n", [Name])
    end,
    {ok, State};

handle_event(Command , State) ->
    io:format("Received unknown command: ~p~n", [Command]),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

