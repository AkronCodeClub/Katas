# Bowling Kata
## Erlang solution
### [bowling.erl](bowling.erl)

This script takes advantage of Erlang's tail-recursion and pattern matching. Pattern matching works by checking each function signature for a match, stopping at the first one that matches, and running the code in that function. The easiest way to see how it works is to go through a couple examples:

Function Call | Matching Line | Result
---|---|---
`score([3,5,7,1])` | 7 | Start recursion with Score=0 and Frame=1.
`score(0, 1, [3,5,7,1])` | 18 | Add to Score, advance Frame, process remaining balls.
`score(8, 2, [7,1])` | 18 | Add to Score, advance Frame, process remaining balls, which is now empty.
`score(16, 3, [])` | 10 | Done with list. Return the Score -> 16.

Function Call | Matching Line | Result
---|---|---
`score([3,7,1])` | 7 | Start recursion with Score=0 and Frame=1.
`score(0, 1, [3,7,1])` | 16 | Spare. The score calculation includes the bonus ball.
`score(11, 2, [1])` | 20 | Frame is not finished. No problem.
`score(12, 3, [])` | 10 | Done with list. Return the Score -> 12.

Function Call | Matching Line | Result
---|---|---
`score([10,10,10])` | 7 | Start recursion with Score=0 and Frame=1.
`score(0, 1, [10,10,10])` | 13 | Strike. The score calculation includes two bonus balls.
`score(30, 2, [10,10])` | 14 | Another strike, but only one bonus ball so far.
`score(50, 3, [10])` | 20 | No bonus balls thrown yet. Treat as an open frame.
`score(60, 4, [])` | 10 | End of list. Return the Score -> 60.

Function Call | Matching Line | Result
---|---|---
`...` | | Fast forward to 9th frame.
`score(240, 9, [10,9,1,8])` | 13 | Strike. Add in two bonus balls.
`score(260, 10, [9,1,8])` | 16 | Spare in 10th frame. Don't forget bonus ball.
`score(278, 11, [8])` | 11 | 11th frame? That 8 is the 10th frame's bonus ball, not the beginning of the next frame. We're done, so report the Score -> 278.
