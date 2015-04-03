-module(goose_game_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([add_player/1]).

%% ct.
all() ->
    [{group, player_api}].

groups() ->
    Tests = [
        add_player
    ],
    [{player_api, [parallel], Tests}].

init_per_suite(_Config) ->
    start_deps(),
    error_logger:tty(false),
    application:start(goose_game),
    [].

end_per_suite(_Config) ->
    application:stop(goose_game),
    stop_deps(),
    ok.

init_per_group(player_api, Config) ->
    [Config].

end_per_group(player_api, _Config) ->
    ok.

add_player(_Config) ->
    Name = joe,
    goose_game_client:add_player(Name),
    timer:sleep(1000),
    true = erlang:is_pid(gproc:where({n,l,{player, Name}})),
    ok.

start_deps() ->
    application:start(gproc).

stop_deps() ->
    application:stop(gproc).


