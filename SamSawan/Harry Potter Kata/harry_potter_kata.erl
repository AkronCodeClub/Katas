-module(harry_potter_kata).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_PRICE, 8).

-export([price/1]).

price(NumberList) ->
    discount_lists(NumberList, []).

discount_lists([], DiscountedLists) ->
    calculate_cost(DiscountedLists, 0);
discount_lists(MasterList, DiscountedLists) ->
    discount_lists(lists:usort(MasterList), MasterList, DiscountedLists).

discount_lists(SingleUniqueBook, MasterList, DiscountedLists) when length(SingleUniqueBook) =:= 1 ->
    calculate_cost(DiscountedLists, length(MasterList) * ?DEFAULT_PRICE);
discount_lists(NewDiscountedListOfBooks, MasterList, DiscountedLists) ->
    discount_lists(delete_repeated_books(MasterList, NewDiscountedListOfBooks), [NewDiscountedListOfBooks|DiscountedLists]).

delete_repeated_books(ListToBeDeleted, []) -> ListToBeDeleted;
delete_repeated_books(ListToBeDeleted, [H|T]) ->
    delete_repeated_books(lists:delete(H, ListToBeDeleted), T).

calculate_cost([], Cost) ->
    Cost;
calculate_cost([DiscountList|RestOfTheDiscountLists], Cost) ->
    calculate_cost(RestOfTheDiscountLists, (?DEFAULT_PRICE*calculate_discount(length(DiscountList))) + Cost).

calculate_discount(N) when N =< 3 ->
    N * (1 - ((N - 1) * 0.05));
calculate_discount(N) ->
    N * (1 - ((N - 1) * 0.05) - 0.05).

-ifdef(EUNIT).

no_books_return_zero_test() ->
    ?assertEqual(0, price([])).

one_book_test_() ->
    [
        ?_assertEqual(8, price([0])),
        ?_assertEqual(8, price([1])),
        ?_assertEqual(8, price([2])),
        ?_assertEqual(8, price([3])),
        ?_assertEqual(8, price([4]))
    ].

two_first_books_returns_sixteen_test() ->
    ?assertEqual(16, price([0,0])).

two_different_books_return_a_slight_discount_test() ->
    ?assertEqual(8*2*0.95, price([0,1])).

three_different_books_return_a_slight_discount_test() ->
    ?assertEqual(8*3*0.90, price([0,1,2])).

combo_breaker_test() ->
    ?assertEqual((8*3*0.90)+8, price([0,0,1,2])).

cool_kids_test() ->
    ?assertEqual((8*3*0.90)+(8*2*0.95), price([0,0,1,2,1])).

cool_kids_two_test() ->
    ?assertEqual((8*5*0.75), price([0,1,2,3,4])).

cool_kids_three_test() ->
    ?assertEqual((8*4*0.80) + (8*3*0.90), price([0,1,2,3,0,1,2])).

lets_go_nuts_test() ->
    Answer = (8*5*0.75) + 2*(8*3*0.90) + (8*2*0.95) + 16,
    ?assertEqual(Answer, price([0,1,2,3,4,0,1,2,0,1,2,0,1,0,0])).

-endif.