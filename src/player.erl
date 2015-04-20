-module(player).
-behaviour(gen_server).

-export([start/1, move/2, load_from_events/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(KEY(Name), {n, l, {?MODULE, Name}}).
-define(TIMEOUT, 10000).

-record(state, {name, events, position}).
-record(player_created, {name, date_created}).
-record(player_moved, {name, steps, date_command}).

start(Name) ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    gen_server:call(Pid, {create, Name}),
    gen_server:call(Pid, process_unsaved_events).

move(Pid, Steps) ->
    gen_server:call(Pid, {move, Steps}),
    gen_server:call(Pid, process_unsaved_events).

load_from_events(Events) ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    gen_server:call(Pid, {replay_events, Events}),
    Pid.

init([]) ->
    {ok, #state{position=0, events=[]}, ?TIMEOUT}.

handle_call({create, Name}, _From, State) ->
    Event = #player_created{name=Name, date_created=erlang:localtime()},
    NewState = apply_new_event(Event, State),
    {reply, ok, NewState, ?TIMEOUT};

handle_call({move, Steps}, _From, #state{name=Name}=State) ->
    Event = #player_moved{name=Name, steps=Steps, date_command=erlang:localtime()},
    NewState = apply_new_event(Event, State),
    {reply, ok, NewState, ?TIMEOUT};

handle_call({replay_events, Events}, _From, State) ->
    NewState = handle_replay_events(Events, State),
    {reply, ok, NewState, ?TIMEOUT};

handle_call(process_unsaved_events, _From, State) ->
    NewState = handle_unsaved_events(State),
    {reply, ok, NewState, ?TIMEOUT};

handle_call(_Request, _From, State) ->
    {reply, ok, State, ?TIMEOUT}.

handle_cast(_Msg, State) ->
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

apply_new_event(Event, State) ->
    NewState = apply_event(Event, State),
    NewEvents = [Event | NewState#state.events],
    NewState#state{events=NewEvents}.

apply_event(#player_created{name=Name, date_created=_DateCreated}, State) ->
    case gproc:where(?KEY(Name)) of
        undefined ->
            gproc:reg(?KEY(Name)),
            gproc:await(?KEY(Name));
        Pid -> Pid
    end,
    State#state{name=Name};

apply_event(#player_moved{steps=Steps}, #state{position=Position} = State) ->
    NewPosition = Position + Steps,
    State#state{position=NewPosition}.

handle_unsaved_events(#state{name=Name, events=Events} = State) ->
    event_store:save(Name, Events),
    State#state{events=[]}.

handle_replay_events([], State) ->
    State;

handle_replay_events([Event|Events], State) ->
    NewState = apply_event(Event, State),
    handle_replay_events(Events, NewState).
