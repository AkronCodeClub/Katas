-module(bowling).

-export([score/1]).

-include_lib("eunit/include/eunit.hrl").

score(Balls) when is_binary(Balls) -> score(parse_string_to_list_of_throws(Balls));
score(Balls) -> score(0, 1, Balls).

% exit conditions
score(ScoreSoFar, _,     [])                                                   -> ScoreSoFar;
score(ScoreSoFar, 11,    _)                                                    -> ScoreSoFar;
% strikes
score(ScoreSoFar, Frame, [10,BonusRoll1,BonusRoll2|Tail])                      -> score(ScoreSoFar +10 +BonusRoll1 +BonusRoll2, Frame+1, [BonusRoll1 , BonusRoll2 | Tail]);
score(ScoreSoFar, Frame, [10,BonusRoll1])                                      -> score(ScoreSoFar +10 +BonusRoll1,             Frame+1, [BonusRoll1]);
% spares
score(ScoreSoFar, Frame, [Roll1,Roll2,BonusRoll1|Tail]) when Roll1+Roll2 == 10 -> score(ScoreSoFar +Roll1+Roll2 +BonusRoll1,    Frame+1, [BonusRoll1 | Tail]);
% open frames
score(ScoreSoFar, Frame, [Roll1,Roll2|Tail])                                   -> score(ScoreSoFar +Roll1+Roll2,                Frame+1, Tail);
% incomplete frame
score(ScoreSoFar, Frame, [Roll1])                                              -> score(ScoreSoFar +Roll1,                      Frame+1, []).

parse_string_to_list_of_throws(Balls) when is_binary(Balls) ->
    lists:reverse(
      lists:foldl(
        fun($|, Acc)          -> Acc;               % frame delimiter: ignore it.
           ($-, Acc)          -> [0 | Acc];         % gutter ball: 0
           ($/, [Prev|_]=Acc) -> [10 - Prev | Acc]; % spare: 10 - previous throw
           ($X, Acc)          -> [10 | Acc];        % strike: 10
           (H, Acc)           -> [H - $0 | Acc]     % 1-9: convert from string to integer.
        end,
        [],
        binary_to_list(Balls))).

% To test, in Erlang shell,          1> c(bowling).
%                                    2> bowling:test().

% To test, in bash or cmd shell,     $ erlc bowling.erl
%                                    $ erl -s bowling test -s init stop -noinput

% To test, in vim,                   :! (erlc % && erl -s %:r test -s init stop -noinput)

before_game_test() ->                        ?assertEqual(  0, score([])).
good_1st_ball_test() ->                      ?assertEqual(  8, score([8,0])).
open_frame_test() ->                         ?assertEqual(  8, score([3,5])).
spare_test() ->                              ?assertEqual( 14, score([4,6,  2,0])).
lucky_spare_test() ->                        ?assertEqual( 18, score([0,10, 2,4])).
pending_spare_test() ->                      ?assertEqual( 13, score([1,2,  4,6])).
spare_and_full_fame_test() ->                ?assertEqual( 23, score([3,7,  4,5])).
strike_test() ->                             ?assertEqual( 42, score([10,   10,   4,0])).
one_pending_strike_test() ->                 ?assertEqual( 10, score([10])).
two_pending_strikes_test() ->                ?assertEqual( 30, score([10,   10])).
three_pending_strikes_test() ->              ?assertEqual( 60, score([10,   10,   10])).
incomplete_open_final_frame_test() ->        ?assertEqual(  6, score([1,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  5])).
open_final_frame_test() ->                   ?assertEqual(  9, score([1,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  5,3])).
incomplete_spare_final_frame_test() ->       ?assertEqual( 11, score([1,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  5,5])).
spare_final_frame_test() ->                  ?assertEqual( 16, score([0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  3,7,6])).
incomplete_strike_final_frame_test() ->      ?assertEqual( 11, score([1,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  10])).
almost_complete_strike_final_frame_test() -> ?assertEqual( 18, score([1,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  10,7])).
strike_final_frame_test() ->                 ?assertEqual( 16, score([0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  10,5,1])).
strike_perfect_final_frame_test() ->         ?assertEqual( 30, score([0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  10,10,10])).
incomplete_frame_test() ->                   ?assertEqual( 12, score([3,4,  5])).
really_bad_game_test() ->                    ?assertEqual(  0, score([0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0,  0,0])).
full_game_1_test() ->                        ?assertEqual(187, score([10,   9,1,  5,5,  7,2,  10,   10,   10,   9,0,  8,2,  9,1,10])).
full_game_2_test() ->                        ?assertEqual(155, score([3,5,  10,   3,7,  8,1,  10,   10,   6,2,  5,4,  7,3,  10,6,3])).
full_game_3_test() ->                        ?assertEqual( 53, score([1,0,  1,1,  0,6,  5,3,  0,6,  0,0,  5,2,  7,1,  0,6,  3,6])).
perfect_game_test() ->                       ?assertEqual(300, score([10,   10,   10,   10,   10,   10,   10,   10,   10,   10,10,10])).
first_ball_only_test() ->                    ?assertEqual(  4, score([4])).

open_frame_parse_string_test() ->            ?assertEqual([1,2,3,4], parse_string_to_list_of_throws(<<"12|34">>)).
another_open_frame_parse_string_test() ->    ?assertEqual([2,3,4,5], parse_string_to_list_of_throws(<<"23|45">>)).
gutterball_parse_string_test() ->            ?assertEqual([1,0,0,4], parse_string_to_list_of_throws(<<"1-|-4">>)).
spare_parse_string_test() ->                 ?assertEqual([1,9,0,4], parse_string_to_list_of_throws(<<"1/|-4">>)).
strike_parse_string_test() ->                ?assertEqual([10,4,2], parse_string_to_list_of_throws(<<"X|42">>)).

calculate_score_from_string_test_() ->      [?_assertEqual(187, score(<<"X|9/|5/|72|X|X|X|9-|8/|9/||X">>)),
                                             ?_assertEqual(300, score(<<"X|X|X|X|X|X|X|X|X|X||XX">>)),
                                             ?_assertEqual(90, score(<<"9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||">>)),
                                             ?_assertEqual(150, score(<<"5/|5/|5/|5/|5/|5/|5/|5/|5/|5/||5">>)),
                                             ?_assertEqual(167, score(<<"X|7/|9-|X|-8|8/|-6|X|X|X||81">>))].
