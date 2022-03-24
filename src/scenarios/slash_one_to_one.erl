-module(slash_one_to_one).

-export([init/0]).
-export([start/1]).

init() ->
  ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
  io:fwrite("My Id ~s", MyId),
  ok.