-module (counter_get_value_SUITE).

-include_lib ("common_test/include/ct.hrl").

-export ([all/0]).

-export ([simple_get_value_test/1]).
 
all() -> [simple_get_value_test].

-define (N, 100).
-define (SPAWNS, 10).

simple_get_value_test (_Config) ->
  ok = application:start (counter),
  lists:foreach (fun (_) -> counter_srv:incr() end, lists:seq(1, ?N)),
  ?N = counter_srv:get_value(),
  timer:sleep (59 * 1000),
  ?N = counter_srv:get_value(),
  timer:sleep (1 * 1000),
  true = (?N /= counter_srv:get_value()),
  application:stop (counter).
