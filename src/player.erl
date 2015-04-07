-module(player).
-export([start/2, init/0]).

-record(state, {name, events}).
-record(player_created, {name, date_created}).

-define(TIMEOUT, 3000).
-define(KEY(Name), {n, l, {?MODULE, Name}}).

start(Name, []) ->
    Pid = spawn(?MODULE, init, []),
    Pid ! {attempt_command, {create, Name}},
    Pid ! process_unsaved_events;

start(_Name, Events) ->
    Pid = spawn(?MODULE, init, []),
    Pid ! {replay_events, Events}.

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
        {replay_events, Events} ->
            NewState = handle_replay_events(Events, State),
            loop(NewState);
        Msg ->
            handle_unknown_message(Msg, State),
            loop(State)
    after ?TIMEOUT ->
            ok
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
            gproc:reg(?KEY(Name)),
            gproc:await(?KEY(Name));
        Pid -> Pid
    end,
    State#state{name=Name}.

handle_unsaved_events(#state{name=Name, events=Events} = State) ->
    io:format("Player ~p is processing unsaved events ~p~n", [Name, Events]),
    event_store:save(Name, Events),
    State#state{events=[]}.

handle_unknown_message(Msg, #state{name=Name, events=Events} = _State) ->
    io:format("Player ~p received message ~p~n", [Name, Msg]),
    io:format("Player ~p has events ~p~n", [Name, Events]).

handle_replay_events([], State) ->
    State;

handle_replay_events([Event|Events], State) ->
    io:format("Event ~p on state ~p~n", [Event, State]),
    NewState = apply_new_event(Event, State),
    handle_replay_events(Events, NewState).
