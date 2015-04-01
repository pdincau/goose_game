-module(players).
-export([find/1]).

find(Name) ->
    case gproc:where({n, l, {player, Name}}) of
        undefined ->
            {error, not_found};
        Pid ->
            {ok, Pid}
    end.
