-module(harry_potter).
-export([expecto_patronum/1, create_sets/1, discount/1, price/1]).

expecto_patronum(BookList) ->
  BooksOrdered = lists:map(fun(BookId) -> lists:filter(fun(Id) -> 
                                                          Id == BookId 
                                                       end, BookList) 
                           end, 
                           [1,2,3,4,5]),
  BookSets = create_sets(BooksOrdered),
  lists:foldl(fun(BookSet, Sum) -> Sum + price(BookSet) end, 0.0, BookSets).

create_sets([[X | Xs] | Xss]) ->
    [[X | [H || [H | _] <- Xss]]
     | create_sets([Xs | [T || [_ | T] <- Xss]])];
create_sets([[] | Xss]) -> create_sets(Xss);
create_sets([]) -> [].

discount(BookSet) when is_list(BookSet) ->
  discount(length(BookSet));
discount(5) -> 0.25;
discount(4) -> 0.2;
discount(3) -> 0.1;
discount(2) -> 0.05;
discount(_) -> 0.

price(BookSet) ->
  8.0 * length(BookSet) * (1.0 - discount(BookSet)).

