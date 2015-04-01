-module(player).
-export([start/1, loop/1]).

start(Name) ->
    Pid = spawn(?MODULE, loop, [Name]),
    gproc:reg({n, l, {?MODULE, Name}}, Pid),
    gproc:await({n,l, {?MODULE, Name}}).

loop(Name) ->
    receive
        Msg ->
            io:format("Player ~p received message ~p~n", [Name, Msg]),
            loop(Name)
    end.

