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
    Events = event_store:get(Name),
    case Events of
        [] ->
            {error, not_found};
        _ ->
            Pid = player:reload(Events),
            {ok, Pid}
    end.
