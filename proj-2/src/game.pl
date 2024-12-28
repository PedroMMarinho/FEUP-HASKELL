:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system)).
:- consult(board).
:- consult(utils).
:- consult(display).
:- consult(value).
:- consult(app).

% Add current_directory(_, 'your_path_here') to set the path to the menus folder, so assets can be loaded correctly
play:-
    main.

/*
* GameConfig = [GameMode, GameDifficulty]
* Size = (4,5,6) with 5 being the only available size for the game
* GameMode => ['player-player', 'player-computer', 'computer-player']
* GameDifficulty => [1, 2]
* GameMode = 'computer-computer' => Bot1Difficulty = [1, 2], Bot2Difficulty = [1, 2]
*/
game(GameConfig,Size):-
    initial_state(GameConfig, GameState),
    game_cycle(GameConfig, GameState, 'none').


initial_state(['player-player', _], [Board, Player, 27, 27, ValidMoves]) :-
    initial_board(Board),
    initial_valid_moves(ValidMoves),
    Player = 'white'.

initial_state(['player-computer', GameDifficulty], [Board, Player, 27, 27, ValidMoves]) :-
    initial_board(Board),
    initial_valid_moves(ValidMoves),
    Player = 'white'.
    
initial_state(['computer-player', GameDifficulty], [Board, Player, 27, 27, ValidMoves]) :-
    initial_board(Board),
    initial_valid_moves(ValidMoves),
    Player = 'white'.

initial_state(['computer-computer', Computer1Difficulty, Computer2Difficulty], [Board, Player, 27, 27, ValidMoves]) :-
    initial_board(Board),
    initial_valid_moves(ValidMoves),
    Player = 'white'.


/*
* Displays the current state of the game:
* display_game(+GameState)
* GameState: A list containing the current board, the current player, the remaining blocks for white and black, and the valid moves.
* This function clears the console and then prints the game information, including the board, the current player's turn, 
* the number of blocks each player has left, and the valid moves available.
*/
display_game([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves]) :-
    clear_console,
    print_header(WhiteBlocks, BlackBlocks),
    print_board(Board),
    print_player_turn(Player),
    print_valid_moves(ValidMoves).

/*
* Executes a move in the game, updating the game state:
* move(+GameState, +Move, -NewGameState)
* GameState: The current state of the game, including the board, current player, and other relevant details.
* Move: The move to be executed, specified as [Row, Col, Direction].
* NewGameState: The resulting game state after applying the move.
* 
* This predicate validates the move for the current player, places the block on the board, 
* updates the remaining blocks, calculates valid moves for the next player, and switches turns.
* If the move is invalid, the game state remains unchanged.
*/

move([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], NewGameState) :-
    member([Row, Col], ValidMoves),
    put_block(Board, Row, Col, 'white', NewBoard, Direction),
    NewWhiteBlocks is WhiteBlocks - 1,
    valid_moves([NewBoard, 'black', NewWhiteBlocks, BlackBlocks, []], NewValidMoves),
    NewGameState = [NewBoard, 'black', NewWhiteBlocks, BlackBlocks, NewValidMoves],
    !.
move([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], NewGameState) :-
    member([Row, Col], ValidMoves),
    put_block(Board, Row, Col, 'black', NewBoard, Direction),
    NewBlackBlocks is BlackBlocks - 1,
    valid_moves([NewBoard, 'white', WhiteBlocks, NewBlackBlocks, []], NewValidMoves),
    NewGameState = [NewBoard, 'white', WhiteBlocks, NewBlackBlocks, NewValidMoves],
    !.
move(GameState, _, GameState) :- !.

/*
* Determines the list of valid moves for the current game state:
* valid_moves(+GameState, -ListOfMoves)
* GameState: The current state of the game, represented as a list containing the board and other game details.
* ListOfMoves: The resulting list of valid moves for the current player.
* 
* The predicate computes all valid moves by iterating through the board and evaluating potential moves
* based on specific conditions defined for each cell type.
*/
valid_moves([Board, _, _, _, _], ListOfMoves) :-
    valid_moves_aux(Board, Board, 10, [], ReversedListOfMoves),
    reverse(ReversedListOfMoves, ListOfMoves).

/*
* Auxiliary predicate to calculate valid moves for the entire board:
* valid_moves_aux(AccumulatedMoves, -ListOfMoves)
* OriginalBoard: The original game board (remains unchanged for context during recursion).
* CurrentBoard: The portion of the board being processed (remaining rows).
* Row: The current row being evaluated.
* AccumulatedMoves: The list of moves accumulated so far.
* ListOfMoves: The final list of valid moves for the entire board.
*/

valid_moves_aux(_, [],  _, AccumulatedMoves, AccumulatedMoves) :- !.
valid_moves_aux(Board, [Line | Rest], Row, AccumulatedMoves, ListOfMoves) :-
    valid_moves_row(Board, Line, Row, 1, AccumulatedMoves, NewAccumulatedMoves),
    NewRow is Row - 1,
    valid_moves_aux(Board, Rest, NewRow, [], NewListOfMoves),
    append(NewAccumulatedMoves, NewListOfMoves, ListOfMoves).

valid_moves_row(_, [], _, _, AccumulatedMoves, AccumulatedMoves) :- !.
valid_moves_row(_, _, 10, _, AccumulatedMoves, AccumulatedMoves) :- !.
valid_moves_row(_, _, _, 10, AccumulatedMoves, AccumulatedMoves) :- !.
valid_moves_row(Board, [[_, 0] | Line], Row, Col, AccumulatedMoves, ListOfMoves) :-
    Row < 10,
    Col < 10,
    NewCol is Col + 1,
    Row mod 2 =:= 1,
    Col mod 2 =:= 1,
    valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves).
valid_moves_row(Board, [[_, 1] | Line], Row, Col, AccumulatedMoves, ListOfMoves) :-
    Row < 10,
    Col < 10,
    NewCol is Col + 1,
    RowIndex is 10 - Row,
    ColIndex is Col - 1,
    RowIndex2 is RowIndex - 1,
    ColIndex2 is ColIndex + 1,
    nth0(RowIndex, Board, Temp),
    nth0(ColIndex2, Temp, [_, Height2]),
    nth0(RowIndex2, Board, Temp2),
    nth0(ColIndex, Temp2, [_, Height3]),
    nth0(ColIndex2, Temp2, [_, Height4]),
    Height2 =:= 1,
    Height3 =:= 1,
    Height4 =:= 1,
    Row > 1, 
    Row < 9, 
    Col > 1, 
    Col < 9, 
    Row mod 2 =:= 0, 
    Col mod 2 =:= 0,
    valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves).
valid_moves_row(Board, [[_, 2] | Line], Row, Col, AccumulatedMoves, ListOfMoves) :-
    Row < 10,
    Col < 10,
    NewCol is Col + 1,
    RowIndex is 10 - Row,
    ColIndex is Col - 1,
    RowIndex2 is RowIndex - 1,
    ColIndex2 is ColIndex + 1,
    nth0(RowIndex, Board, Temp),
    nth0(ColIndex2, Temp, [_, Height2]),
    nth0(RowIndex2, Board, Temp2),
    nth0(ColIndex, Temp2, [_, Height3]),
    nth0(ColIndex2, Temp2, [_, Height4]),
    Height2 =:= 2,
    Height3 =:= 2,
    Height4 =:= 2,
    Row > 2, 
    Row < 8, 
    Col > 2, 
    Col < 8, 
    Row mod 2 =:= 1, 
    Col mod 2 =:= 1,
    valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves).
valid_moves_row(Board, [[_, 3] | Line], Row, Col, AccumulatedMoves, ListOfMoves) :-
    Row < 10,
    Col < 10,
    NewCol is Col + 1,
    RowIndex is 10 - Row,
    ColIndex is Col - 1,
    RowIndex2 is RowIndex - 1,
    ColIndex2 is ColIndex + 1,
    nth0(RowIndex, Board, Temp),
    nth0(ColIndex2, Temp, [_, Height2]),
    nth0(RowIndex2, Board, Temp2),
    nth0(ColIndex, Temp2, [_, Height3]),
    nth0(ColIndex2, Temp2, [_, Height4]),
    Height2 =:= 3,
    Height3 =:= 3,
    Height4 =:= 3,
    Row > 3,
    Row < 7,
    Col > 3,
    Col < 7,
    Row mod 2 =:= 0,
    Col mod 2 =:= 0,
    valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves).
valid_moves_row(Board, [[_, 4] | Line], Row, Col, AccumulatedMoves, ListOfMoves) :-
    Row < 10,
    Col < 10,
    NewCol is Col + 1,
    RowIndex is 10 - Row,
    ColIndex is Col - 1,
    RowIndex2 is RowIndex - 1,
    ColIndex2 is ColIndex + 1,
    nth0(RowIndex, Board, Temp),
    nth0(ColIndex2, Temp, [_, Height2]),
    nth0(RowIndex2, Board, Temp2),
    nth0(ColIndex, Temp2, [_, Height3]),
    nth0(ColIndex2, Temp2, [_, Height4]),
    Height2 =:= 4,
    Height3 =:= 4,
    Height4 =:= 4,
    Row =:= 5,
    Col =:= 5,
    valid_moves_row(Board, Line, Row, NewCol, [[Row, Col] | AccumulatedMoves], ListOfMoves).
valid_moves_row(Board, [[_, _] | Line], Row, Col, AccumulatedMoves, ListOfMoves) :-
    Row < 10,
    Col < 10,
    NewCol is Col + 1,
    valid_moves_row(Board, Line, Row, NewCol, AccumulatedMoves, ListOfMoves).

/*
* Determines if the game is over and identifies the winner:
* game_over(+GameState, -Winner)
* GameState: The current state of the game, represented as a list containing the board and other game details.
* Winner: The winner of the game, either 'white', 'black', or 'none' if the game is not over.
* 
* The predicate checks two conditions:
* 1. If either player has achieved a winning path.
* 2. If one of the players runs out of blocks.
* If any of these conditions are met, the game ends, and the corresponding winner is determined.
*/
game_over([Board, _, _, _, _], 'white') :-
    find_path(Board, 'white', true),
    !.
game_over([Board, _, _, _, _], 'black') :-
    find_path(Board, 'black', true),
    !.
game_over([_, _, 0, _, _], 'black') :- !.
game_over([_, _, _, 0, _], 'white') :- !.
game_over(_, 'none').


find_path(Board, 'white', true) :-
    between(0, 9, Start),  % Check from positions 0 to 9
    find_path_aux(Board, 0, Start, [], 'white', true, true), !.
find_path(Board, 'black', true) :-
    between(0, 9, Start),  % Check from positions 0 to 9
    find_path_aux(Board, Start, 0, [], 'black', true, true), !.
find_path(_, _, false).

find_path_aux(Board, 9, Col, _, 'white', true, true) :-
    nth0(9, Board, Line),
    nth0(Col, Line, ['white', _]),
    !.
find_path_aux(Board, Row, 9, _, 'black', true, true) :-
    nth0(Row, Board, Line),
    nth0(9, Line, ['black', _]),
    !.
find_path_aux(_, _, _, _, _, false, _) :- fail.
find_path_aux(Board, Row, Col, Visited, Player, true, PathFound) :-
    % Check bounds
    Row > -1, Row < 10,
    Col > -1, Col < 10,

    % Check if the current cell has been visited
    \+ member([Row, Col], Visited),

    % Mark the current cell as visited
    NewVisited = [[Row, Col] | Visited],

    % Get the color of the current cell
    nth0(Row, Board, Line),
    nth0(Col, Line, [Player, _]),

    % Calculate neighboring cells
    Row1 is Row - 1,
    Row2 is Row + 1,
    Col1 is Col - 1,
    Col2 is Col + 1,

    % Explore the four neighboring cells
    (find_path_aux(Board, Row, Col1, NewVisited, Player, true, PathFound) ;
     find_path_aux(Board, Row, Col2, NewVisited, Player, true, PathFound) ;
     find_path_aux(Board, Row1, Col, NewVisited, Player, true, PathFound) ;
     find_path_aux(Board, Row2, Col, NewVisited, Player, true, PathFound)).

find_path_aux(Board, Row, Col, Visited, Player, true, PathFound) :-
    % Check bounds
    Row > -1, Row < 10,
    Col > -1, Col < 10,

    % Check if the current cell has been visited
    \+ member([Row, Col], Visited),

    % Mark the current cell as visited
    NewVisited = [[Row, Col] | Visited],

    % Get the color of the current cell
    nth0(Row, Board, Line),
    nth0(Col, Line, [Color, _]),

    % Check if the color is different from the player
    Color \= Player,

    % Calculate neighboring cells
    Row1 is Row - 1,
    Row2 is Row + 1,
    Col1 is Col - 1,
    Col2 is Col + 1,

    % Explore the four neighboring cells
    (find_path_aux(Board, Row, Col1, NewVisited, Player, false, PathFound) ;
     find_path_aux(Board, Row, Col2, NewVisited, Player, false, PathFound) ;
     find_path_aux(Board, Row1, Col, NewVisited, Player, false, PathFound) ;
     find_path_aux(Board, Row2, Col, NewVisited, Player, false, PathFound)).

% value(GameState, Player, Value)
% If value < 0 the player is losing
% If value == 0 the player is drawing
% If value > 0 the player is winning
value([Board, _, _, _, _], 'white', Value) :-
    get_value(Board, 'white', WhiteValue),
    get_value(Board, 'black', BlackValue),
    Value is WhiteValue - BlackValue.
value([Board, _, _, _, _], 'black', Value) :-
    get_value(Board, 'white', WhiteValue),
    get_value(Board, 'black', BlackValue),
    Value is BlackValue - WhiteValue.

% choose_move(GameState, Level, Move)
choose_move([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], 1, [Row, Col, Direction]) :-
    length(ValidMoves, Length),
    random(0, Length, Index),
    nth0(Index, ValidMoves, [Row, Col]),
    random(1, 3, Direction), 
    sleep(0.2),
    !.
choose_move([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], 2, Move) :-
    choose_move_aux([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], -100, [], [Row1, Col1, Direction1]),
    put_block(Board, Row1, Col1, 'black', NewBoard, Direction1),
    find_path(NewBoard, 'black', true),
    choose_move_aux([Board, 'white', WhiteBlocks, BlackBlocks, [[Row1, Col1]]], -100, [], Move),
    !.
choose_move([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], 2, Move) :-
    choose_move_aux([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], -100, [], [Row1, Col1, Direction1]),
    put_block(Board, Row1, Col1, 'white', NewBoard, Direction1),
    find_path(NewBoard, 'white', true),
    choose_move_aux([Board, 'black', WhiteBlocks, BlackBlocks, [[Row1, Col1]]], -100, [], Move),
    !.
choose_move(GameState, 2, Move) :-
    choose_move_aux(GameState, -100, [], Move),
    !.
choose_move_aux([_, _, _, _, []], _, CurrentBestMove, CurrentBestMove) :- !.
choose_move_aux([Board, Player, WhiteBlocks, BlackBlocks, [[Row, Col] | Rest]], Max, CurrentBestMove, BestMove) :-
    put_block(Board, Row, Col, Player, NewBoard1, 1),
    value([NewBoard1, Player, WhiteBlocks, BlackBlocks, [[Row, Col] | Rest]], Player, Value1),
    put_block(Board, Row, Col, Player, NewBoard2, 2),
    value([NewBoard2, Player, WhiteBlocks, BlackBlocks, [[Row, Col] | Rest]], Player, Value2),
    Value1 >= Value2,
    Value1 > Max,
    choose_move_aux([Board, Player, WhiteBlocks, BlackBlocks, Rest], Value1, [Row, Col, 1], BestMove),
    !.
choose_move_aux([Board, Player, WhiteBlocks, BlackBlocks, [[Row, Col] | Rest]], Max, CurrentBestMove, BestMove) :-
    put_block(Board, Row, Col, Player, NewBoard1, 1),
    value([NewBoard1, Player, WhiteBlocks, BlackBlocks, [[Row, Col] | Rest]], Player, Value1),
    put_block(Board, Row, Col, Player, NewBoard2, 2),
    value([NewBoard2, Player, WhiteBlocks, BlackBlocks, [[Row, Col] |Rest]], Player, Value2),
    Value2 > Value1,
    Value2 > Max,
    choose_move_aux([Board, Player, WhiteBlocks, BlackBlocks, Rest], Value2, [Row, Col, 2], BestMove),
    !.
choose_move_aux([Board, Player, WhiteBlocks, BlackBlocks, [[Row, Col] | Rest]], Max, CurrentBestMove, BestMove) :-
    choose_move_aux([Board, Player, WhiteBlocks, BlackBlocks, Rest], Max, CurrentBestMove, BestMove),
    !.

game_cycle(_, [Board, _, WhiteBlocks, BlackBlocks, _], 'white') :- 
    clear_console,
    print_header(WhiteBlocks, BlackBlocks),
    print_board(Board),
    print_winner_message('white'),
    !.
game_cycle(_, [Board, _, WhiteBlocks, BlackBlocks, _], 'black') :-
    clear_console,
    print_header(WhiteBlocks, BlackBlocks),
    print_board(Board),
    print_winner_message('black'),
    !.
game_cycle(['player-player', _], [Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], _) :-
    get_move([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], Move),
    move([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], Move, [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves]),
    game_over([NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner),
    game_cycle(['player-player', _], [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner).
/*
* GameMode = 'player-computer'
* Player starts first meaning he's the white player
* GameMode = 'computer-player'
* Computer is first meaning he's the white player
*/
game_cycle(['player-computer', GameDifficulty], [Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], _) :-
    get_move([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], Move),
    move([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], Move, [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves]),
    game_over([NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner),
    game_cycle(['player-computer', GameDifficulty], [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner).

game_cycle(['player-computer', GameDifficulty], [Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], _) :-
    choose_move([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], GameDifficulty, Move),
    move([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], Move, [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves]),
    game_over([NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner),
    game_cycle(['player-computer', GameDifficulty], [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner).

game_cycle(['computer-player', GameDifficulty], [Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], _) :-
    choose_move([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], GameDifficulty, Move),
    move([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], Move, [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves]),
    game_over([NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner),
    game_cycle(['computer-player', GameDifficulty], [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner).

game_cycle(['computer-player', GameDifficulty], [Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], _) :-
    get_move([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], Move),
    move([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], Move, [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves]),
    game_over([NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner),
    game_cycle(['computer-player', GameDifficulty], [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner).

% Added sleep to make the game more understandable
game_cycle(['computer-computer', Computer1Difficulty, Computer2Difficulty], [Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], _) :-
    choose_move([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], Computer1Difficulty, Move),
    move([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], Move, [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves]),
    display_game([NewBoard, 'white', NewWhiteBlocks, NewBlackBlocks, ValidMoves]),
    sleep(0.8),
    game_over([NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner),
    game_cycle(['computer-computer', Computer1Difficulty, Computer2Difficulty], [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner).

% Added sleep to make the game more understandable
game_cycle(['computer-computer', Computer1Difficulty, Computer2Difficulty], [Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], _) :-
    choose_move([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], Computer2Difficulty, Move),
    move([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], Move, [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves]),
    display_game([NewBoard, 'black', NewWhiteBlocks, NewBlackBlocks, ValidMoves]),
    sleep(0.8),
    game_over([NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner),
    game_cycle(['computer-computer', Computer1Difficulty, Computer2Difficulty], [NewBoard, NewPlayer, NewWhiteBlocks, NewBlackBlocks, NewValidMoves], Winner).

get_move([_, _, _, _, []], []) :- !.
get_move([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], Move) :-
    nth0(0, ValidMoves, [Row, Col]),
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 1], Move).

get_move_aux([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Move) :-
    member([Row, Col], ValidMoves),
    put_block(Board, Row, Col, 'white_valid', TempBoard, Direction),
    display_game([TempBoard, 'white', WhiteBlocks, BlackBlocks, ValidMoves]),
    print_valid_move_message,
    get_line(Code, []),
    move_logic([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Code, true, Move), !.
get_move_aux([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Move) :-
    member([Row, Col], ValidMoves),
    put_block(Board, Row, Col, 'black_valid', TempBoard, Direction),
    display_game([TempBoard, 'black', WhiteBlocks, BlackBlocks, ValidMoves]),
    print_valid_move_message,
    get_line(Code, []),
    move_logic([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Code, true, Move), !.
get_move_aux([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Move) :-
    \+ member([Row, Col], ValidMoves),
    put_block(Board, Row, Col, 'white_invalid', TempBoard, Direction),
    display_game([TempBoard, 'white', WhiteBlocks, BlackBlocks, ValidMoves]),
    print_invalid_move_message,
    get_line(Code, []),
    move_logic([Board, 'white', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Code, false, Move), !.
get_move_aux([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Move) :-
    \+ member([Row, Col], ValidMoves),
    put_block(Board, Row, Col, 'black_invalid', TempBoard, Direction),
    display_game([TempBoard, 'black', WhiteBlocks, BlackBlocks, ValidMoves]),
    print_invalid_move_message,
    get_line(Code, []),
    move_logic([Board, 'black', WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Code, false, Move), !.

% move_logic(GameState, CurrentMove, Code, ValidMove, Move)
% A - left
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, 1, Direction], 'a', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, 9, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, 1, Direction], 'A', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, 9, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 'a', _, Move) :-
    Col \= 1,
    NewCol is (Col - 1) mod 10, 
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, NewCol, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 'A', _, Move) :-
    Col \= 1,
    NewCol is (Col - 1) mod 10, 
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, NewCol, Direction], Move), !.

% D - right
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, 9, Direction], 'd', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, 1, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, 9, Direction], 'D', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, 1, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 'd', _, Move) :-
    Col \= 9,
    NewCol is (Col + 1) mod 10, 
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, NewCol, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 'D', _, Move) :-
    Col \= 9,
    NewCol is (Col + 1) mod 10, 
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, NewCol, Direction], Move), !.

% W - up
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [9, Col, Direction], 'w', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [1, Col, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [9, Col, Direction], 'W', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [1, Col, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 'w', _, Move) :-
    Row \= 9,
    NewRow is (Row + 1) mod 10,
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [NewRow, Col, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 'W', _, Move) :-
    Row \= 9,
    NewRow is (Row + 1) mod 10,
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [NewRow, Col, Direction], Move), !.

% S - down
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [1, Col, Direction], 's', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [9, Col, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [1, Col, Direction], 'S', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [9, Col, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 's', _, Move) :-
    Row \= 1,
    NewRow is (Row - 1) mod 10,
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [NewRow, Col, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 'S', _, Move) :-
    Row \= 1,
    NewRow is (Row - 1) mod 10,
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [NewRow, Col, Direction], Move), !.

% R - Rotate
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 1], 'r', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 2], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 1], 'R', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 2], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 2], 'r', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 1], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 2], 'R', _, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, 1], Move), !.

% C - Confirm
move_logic(_, Move, 'c', true, Move) :- !.
move_logic(_, Move, 'C', true, Move) :- !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 'c', false, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Move), !.
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], 'C', false, Move) :-
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [Row, Col, Direction], Move), !.

% Specific Coordinates
move_logic([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [_, _, Direction], Code, _, Move) :-
    parse_move_code(Code, NewCol, NewRow),
    NewCol < 10,
    NewRow < 10,
    get_move_aux([Board, Player, WhiteBlocks, BlackBlocks, ValidMoves], [NewRow, NewCol, Direction], Move), !.

% Else
move_logic(GameState, CurrentMove, _, _, Move) :-
    get_move_aux(GameState, CurrentMove, Move), !.

parse_move_code(Code, NewCol, NewRow) :-
    atom_length(Code, 3),       % Check if length is 3
    sub_atom(Code, 1, 1, 1, '-'),  % Ensure the middle character is a hyphen ('-')
    
    % Extract the first (column) and third (row) characters
    sub_atom(Code, 0, 1, _, ColStr),  % First character (column)
    sub_atom(Code, 2, 1, _, RowStr),  % Third character (row)

    % Convert the extracted characters into numbers
    atom_chars(ColStr, [ColChar]),  % Convert atom to list of characters
    number_chars(NewCol, [ColChar]),  % Convert list of characters to number
    
    atom_chars(RowStr, [RowChar]),  % Convert atom to list of characters
    number_chars(NewRow, [RowChar]). % Convert list of characters to number
