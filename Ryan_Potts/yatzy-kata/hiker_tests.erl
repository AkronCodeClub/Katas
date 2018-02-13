-module(hiker_tests).
-include_lib("eunit/include/eunit.hrl").
-import(hiker, [answer/0]).

life_the_universe_and_everything_test() ->
  ?assertEqual(42, hiker:answer()).

is_this_yatzy_test() ->
  ?assertEqual(50, hiker:yatzy([1,1,1,1,1])).
 
is_not_this_yatzy_test() ->
  ?assertEqual(0, hiker:yatzy([1,1,1,1,2])).
  
score_chance_test_() ->
    lists:map(
        fun({List,Score}) ->
            ?_assertEqual(Score,hiker:chance(List))
        end,
        [{[1,2,3,4,5],15},{[1,2,3,4,6], 16},{[1,1,1,1,1],5}]).
        
sum_pips_test_() ->
    lists:map(
        fun({List,Pip,Score}) ->
            ?_assertEqual(Score,hiker:sum_pips(List, Pip))
        end,
        [{[1,2,3,4,5], 1, 1},{[1,2,3,4,6], 5, 0},{[1,1,1,1,1], 1, 5}]).
     
has_pair_test_() ->
    lists:map(
        fun({List, Score}) ->
            ?_assertEqual(Score,hiker:score_pair(List))
        end,
        [{[1,2,1,4,5], 2},{[1,1,1,4,6], 0},{[3,5,3,1,3], 0}]).
  