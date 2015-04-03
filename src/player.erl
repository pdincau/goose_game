-module(player).
-export([start/1, save/1, init/0]).

-record(state, {name, events}).
-record(player_created, {name, date_created}).

start(Name) ->
    Pid = spawn(?MODULE, init, []),
    Pid ! {attempt_command, {create, Name}},
    Pid.

save(Pid) ->
    Pid ! process_unsaved_events,
    ok.

init() ->
    loop(#state{events=[]}).

loop(State) ->
    receive
        {attempt_command, Command} ->
            NewState = attempt_command(Command, State),
            loop(NewState);
        process_unsaved_events ->
            NewState = handle_unsaved_events(State),
            loop(NewState);
        Msg ->
            handle_unknown_message(Msg, State),
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
        Pid -> Pid
    end,
    State#state{name=Name}.

handle_unsaved_events(#state{name=Name, events=Events} = State) ->
    io:format("Player ~p is processing unsaved events ~p~n", [Name, Events]),
    event_store:save(Name, lists:reverse(Events)),
    State#state{events=[]}.

handle_unknown_message(Msg, #state{name=Name, events=Events} = _State) ->
    io:format("Player ~p received message ~p~n", [Name, Msg]),
    io:format("Player ~p has events ~p~n", [Name, Events]).
