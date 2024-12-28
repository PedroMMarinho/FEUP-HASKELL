% Example of an initial board
initial_board(
    [
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['white', 1], ['black', 1], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['black', 1], ['white', 1], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ]]).
% Example of a board where the white player wins
white_winner_board(
    [
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['white', 1], ['black', 1], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['black', 1], ['white', 1], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ]]).
% Another example of a board where the white player wins
white_winner_board2(
    [
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['white', 0], ['white', 0], ['white', 0], ['white', 0], ['white', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['white', 1], ['black', 1], ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['black', 1], ['white', 1], ['empty', 0], ['white', 0], ['white', 0], ['empty', 0] ],
    [ ['white', 0], ['white', 0], ['white', 0], ['empty', 0], ['white', 0], ['white', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0] ],
    [ ['white', 0], ['white', 0], ['white', 0], ['white', 0], ['white', 0], ['white', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0] ],
    [ ['white', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0] ],
    [ ['white', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0] ]]).

% Example of the black player winning
black_winner_board(
    [
    [ ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['white', 1], ['black', 1], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['black', 1], ['white', 1], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['empty', 0], ['empty', 0], ['white', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0], ['empty', 0] ],
    [ ['black', 0], ['black', 0], ['black', 0], ['black', 0], ['black', 0], ['black', 0], ['black', 0], ['black', 0], ['black', 0], ['black', 0] ]]).

% Example of a board where the game ends
final_board(
    [
    [ ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 4], ['empty', 4], ['empty', 4], ['empty', 4], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 4], ['white', 4], ['black', 4], ['empty', 4], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 4], ['black', 4], ['white', 4], ['empty', 4], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 4], ['empty', 4], ['empty', 4], ['empty', 4], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 3], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 2], ['empty', 1] ],
    [ ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1], ['empty', 1] ]]).
% Initial state of a board with size 5x5
initial_valid_moves([[1, 1], [1, 3], [1, 5], [1, 7], [1, 9],
                     [3, 1], [3, 3], [3, 5], [3, 7], [3, 9],
                     [5, 1], [5, 3],         [5, 7], [5, 9],
                     [7, 1], [7, 3], [7, 5], [7, 7], [7, 9],
                     [9, 1], [9, 3], [9, 5], [9, 7], [9, 9]]).
% Final valid move of a board with size 5x5
final_valid_moves([[5,5]]).