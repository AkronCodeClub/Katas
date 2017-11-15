-module(hiker).
-export([answer/0, yatzy/1, chance/1, sum_pips/2, score_pair/1]).

answer() ->
  6 * 7.
  
yatzy([Head|_] = List) ->
  score_yatzy(lists:all(fun(Number) -> Number == Head end, List)).

score_yatzy(true) ->
  50;
score_yatzy(_) ->
  0.

chance(Data) ->
  lists:foldl(fun(Number, Acc) -> Number + Acc end, 0, Data).
 
sum_pips(List, Pip) ->
  lists:foldl(fun(Die, Acc) when Pip == Die  -> Pip + Acc;
                 (_Die, Acc) -> Acc
              end, 0, List).
              
 score_pair(List) ->
   SortedList = lists:sort(List),
   get_pair(SortedList).
   
 get_pair(List) ->
    {_LastDie, Pair} = lists:foldr(fun(Die, {Die, [Die]}) -> {Die, [Die,Die]};
                                      (Die, {Die, [Die,Die]}) -> {Die, []};
                                      (_Die, {LastDie, [LastDie|LastDie] = Pairs}) -> {LastDie, Pairs};
                                      (Die, {_Die, _Pairs}) -> {Die, [Die]}
                end, {0, []}, List),
    case length(Pair) of
      2 ->
        chance(Pair);
      _ ->
        0
    end.
