# Bowling Kata
## SQL solution
### [create\_table.sql](create_table.sql)

This script will create the table you need for the statement to work. It has three `tinyint` columns:

* `frame` is the frame number of the ball thrown. Valid values are 1-10.
* `ball` is the number of the ball thrown within the frame. Valid values are 1 or 2. You cannot have a 2 without a 1 in the same frame. 3 is valid in the 10th frame, if it also has a 1 and a 2.
* `pins` is the number of pins that were knocked down with that ball.

### [bowling.sql](bowling.sql)

This is where the magic is. Here is how it works.

1. `Lines 8-10` calculates the total pins and number of balls thrown in each frame.
1. `Lines 11-13` join the bowling table again to get the next three balls if they are known. These are the bonus balls for spares and strikes.
1. Lines 4 and 5 calculate the bonus points for spares and strikes. For illustration, let the spare or strike occur in **frame 7**.
    1. `Line 4` calculates the bonus for spares (10 pins down with 2 balls thrown). The bonus is **frame 8 ball 1**.
    1. `Line 5` calculates the bonus for strikes (10 pins down with 1 ball thrown). The bonus is **frame 8 ball 1** + either **frame 8 ball 2** or **frame 9 ball 1**.
1. `Line 1` adds the frames' total pins and bonus points, and sums all the frames to get the final score, or current score if the game is incomplete.

### [bowling\_tests.sql](bowling_tests.sql)

This script sets up the data in the table for unit tests. Each test starts with a `delete` statement. `Insert` statements then populate the table in preparation for the query being run. The comment before the `delete` statement shows the expected value.
