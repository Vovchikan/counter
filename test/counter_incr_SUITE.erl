-module (counter_incr_SUITE).

-include_lib ("common_test/include/ct.hrl").

-export ([all/0,
          init_per_testcase/2,
          end_per_testcase/2]).

-export ([simple_incr_test/1,
          spawn_incr_test/1]).
 
all() -> [simple_incr_test,
          spawn_incr_test].

-define (N, 100).
-define (SPAWNS, 10).
 
init_per_testcase (_, Config) ->
  ok = application:start (counter),
  [{app, counter} | Config].
 
end_per_testcase (_, Config) ->
  application:stop (?config(app, Config)).
 
simple_incr_test (_Config) ->
  counter_incr_ntimes (?N),
  ?N = counter_srv:get_value().

spawn_incr_test (_Config) ->
  F = fun (_) -> spawn(fun () -> 
                           counter_incr_ntimes(?N)
                       end) end,
  lists:foreach (F, lists:seq(1, ?SPAWNS)),
  timer:sleep (?SPAWNS * 100), % wait for spawns to finish
  ?N * ?SPAWNS = counter_srv:get_value().


%%==============================================================================
%% Internal functions
%%==============================================================================
  
counter_incr_ntimes (N) ->
  lists:foreach (fun (_) -> counter_srv:incr() end, lists:seq(1, N)).
