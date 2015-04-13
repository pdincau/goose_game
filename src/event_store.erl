-module(event_store).
-export([init/0, save/2, get/1]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

save(Name, Events) ->
    io:format("Saving events ~p for player ~p~n", [Events, Name]),
    StoredEvents = get_raw_events(Name),
    CombinedEvents = Events ++ StoredEvents,
    ets:insert(?TABLE_ID, {Name, CombinedEvents}),
    io:format("Player ~p has now events ~p~n", [Name, CombinedEvents]).


get(Name) ->
    lists:reverse(get_raw_events(Name)).

get_raw_events(Name) ->
    case ets:lookup(?TABLE_ID, Name) of
        [{Name, Events}] -> Events;
        [] -> []
    end.
