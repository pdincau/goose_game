# goose_game

Compile as:

    ./rebar compile

Run as:

    $ erl -pa _build/default/lib/*/ebin
    1> application:start(gproc).
    ok
    2> goose_game:start().
