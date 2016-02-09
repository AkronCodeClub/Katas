-module(harry_potter_tests).
-include_lib("eunit/include/eunit.hrl").
-import(harry_potter, [expecto_patronum/1, create_sets/1, discount/1, price/1]).

expecto_patronum_test_() ->
  [
     ?_assertEqual(8.0, expecto_patronum([1]))
    ,?_assertEqual(16.0, expecto_patronum([1,1]))
   ,?_assertEqual(15.2, expecto_patronum([1,2]))
   ,?_assertEqual(21.6, expecto_patronum([1,3,4]))
   ,?_assertEqual(25.6, expecto_patronum([1,2,3,5]))
   ,?_assertEqual(30.0, expecto_patronum([1,2,3,4,5]))
   ,?_assertEqual(43.2, expecto_patronum([1,2,3,3,2,1]))
   ,?_assertEqual(68.0, expecto_patronum([1,2,3,4,5,1,2,3,3,4,5]))
   ,?_assertEqual(0.0, expecto_patronum([]))
   ,?_assertEqual(0.0, expecto_patronum([6]))
  ].

create_sets_test_() ->
  [
     ?_assertEqual([[1,2]], create_sets([[1],[2],[],[],[]]))
    ,?_assertEqual([[1,2,3]], create_sets([[1],[2],[3],[],[]]))
    ,?_assertEqual([[1,2,3],[2,3],[2]], create_sets([[1],[2,2,2],[3,3],[],[]]))
    ,?_assertEqual([[2,3,5],[2,3],[2]], create_sets([[],[2,2,2],[3,3],[],[5]]))
  ].

discount_test_() ->
  [
    ?_assertEqual(0, discount([1]))
   ,?_assertEqual(0.05, discount([1,2]))
   ,?_assertEqual(0.1, discount([1,3,5]))
   ,?_assertEqual(0.2, discount([1,3,4,5]))
   ,?_assertEqual(0.25, discount([1,2,3,4,5]))
  ].

price_test_() ->
  [
     ?_assertEqual(0.0, price([]))
    ,?_assertEqual(8.0, price([1]))
    ,?_assertEqual(15.2, price([1,2]))
    ,?_assertEqual(21.6, price([3,2,1]))
    ,?_assertEqual(25.6, price([1,2,5,3]))
    ,?_assertEqual(30.0, price([1,2,3,4,5]))
  ].
