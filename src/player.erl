-module(player).
-export([start/1, init/1]).

-record(state, {name}).

start(Name) ->
    Pid = spawn(?MODULE, init, [Name]),
    Pid ! {attempt_command, {create, Name}}.

init(Name) ->
    loop(#state{name=Name}).

loop(State) ->
    receive
        {attempt_command, Command} ->
            attempt_command(Command),
            loop(State);
        Msg ->
            Name = State#state.name,
            io:format("Player ~p received message ~p~n", [Name, Msg]),
            loop(State)
    end.

attempt_command({create, Name}) ->
    gproc:reg({n, l, {?MODULE, Name}}),
    gproc:await({n,l, {?MODULE, Name}}).

