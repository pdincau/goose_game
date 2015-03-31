-module(goose_game_client).

-export([start/0, stop/0]).
-export([add_player/1]).

-record(add_player, {name}).

start() ->
    application:start(goose_game).

stop() ->
    application:stop(goose_game).

add_player(Name) ->
    command_bus:send(#add_player{name=Name}).
