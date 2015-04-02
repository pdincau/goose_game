-module(player).
-export([start/1, init/0]).

-record(state, {name, events}).
-record(player_created, {name, date_created}).

start(Name) ->
    Pid = spawn(?MODULE, init, []),
    Pid ! {attempt_command, {create, Name}}.

init() ->
    loop(#state{events=[]}).

loop(State) ->
    receive
        {attempt_command, Command} ->
            NewState = attempt_command(Command, State),
            loop(NewState);
        Msg ->
            Name = State#state.name,
            Events = State#state.events,
            io:format("Player ~p received message ~p~n", [Name, Msg]),
            io:format("Player ~p has events ~p~n", [Name, Events]),
            loop(State)
    end.

attempt_command({create, Name}, State) ->
    Event = #player_created{name=Name, date_created=erlang:localtime()},
    apply_new_event(Event, State).

apply_new_event(Event, State) ->
    NewState = apply_event(Event, State),
    NewEvents = [Event | NewState#state.events],
    NewState#state{events=NewEvents}.

apply_event(#player_created{name=Name, date_created=_DateCreated}, State) ->
    case gproc:where({n,l, {player, Name}}) of
        undefined ->
            gproc:reg({n, l, {player, Name}}),
            gproc:await({n,l, {player, Name}});
        _ -> ok
    end,
    State#state{name=Name}.
