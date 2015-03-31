-module(command_bus).
-export([start_link/0]).
-export([send/1, add_handler/2]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

send(Command) ->
    gen_event:notify(?MODULE, Command).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

