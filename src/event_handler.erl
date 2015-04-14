-module(event_handler).
-behaviour(gen_event).

-export([subscribe/0]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
-record(player_created, {name, date_created}).

subscribe() ->
    event_bus:add_handler(?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_event(#player_created{name=Name, date_created=_}, State) ->
    io:format("Giocatore ~p si unisce alla partita.~n", [Name]),
    {ok, State};

handle_event(Event , State) ->
    io:format("Received unknown event: ~p~n", [Event]),
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

