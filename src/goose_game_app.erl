-module(goose_game_app).

-behaviour(application).

-export([start/2
        ,stop/1]).

start(_StartType, _StartArgs) ->
    event_store:init(),
    case goose_game_sup:start_link() of
        {ok, Pid} ->
            command_handler:subscribe(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
