-module(command_handler).
-behaviour(gen_event).

-export([subscribe/0]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

subscribe() ->
    command_bus:add_handler(?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_event(Command, State) ->
    io:format("Command was: ~p~n", [Command]),
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

