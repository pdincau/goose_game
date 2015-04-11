-module(goose_game_client).

-export([start/0, stop/0]).
-export([add_player/1, move_player/2]).

-record(add_player, {name}).
-record(move_player, {name, steps}).

start() ->
    application:start(goose_game).

stop() ->
    application:stop(goose_game).

add_player(Name) ->
    command_bus:send(#add_player{name=Name}).

move_player(Name, Steps) ->
    command_bus:send(#move_player{name=Name, steps=Steps}).
