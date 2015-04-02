-module(player).
-export([start/1, loop/1]).

start(Name) ->
    Pid = spawn(?MODULE, loop, [Name]),
    Pid ! {attempt_command, {create, Name}}.

loop(Name) ->
    receive
        {attempt_command, Command} ->
            attempt_command(Command),
            loop(Name);
        Msg ->
            io:format("Player ~p received message ~p~n", [Name, Msg]),
            loop(Name)
    end.

attempt_command({create, Name}) ->
    gproc:reg({n, l, {?MODULE, Name}}),
    gproc:await({n,l, {?MODULE, Name}}).

