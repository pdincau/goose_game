-module(players).
-export([find/1]).

find(Name) ->
    case gproc:where({n, l, {player, Name}}) of
        undefined ->
            load_from_event_store(Name);
        Pid ->
            {ok, Pid}
    end.

load_from_event_store(Name) ->
    case event_store:get(Name) of
        [] ->
            {error, not_found};
        Events ->
            Pid = player:load_from_events(Events),
            {ok, Pid}
    end.
