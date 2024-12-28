:- use_module(library(lists)).
:- consult(utils).
:- consult(display).
:- consult(data).

/*
* Places a piece on the board at the specified row and column:
* put_piece(+Board, +Row, +Col, +Color, -NewBoard)
* Board: The current game board represented as a list of lists.
* Row: The row number (1-based) where the piece should be placed.
* Col: The column number (1-based) where the piece should be placed.
* Color: The color of the piece to be placed on the board (e.g., 'black' or 'white').
* NewBoard: The updated board after placing the piece at the given position.
* 
* The predicate updates the height of the piece at the specified position (Row, Col) 
* and places the piece with the given color at the correct position, 
* updating the board with the new piece placement.
*/
put_piece(Board, Row, Col, Color, NewBoard) :-
    RowIndex is 10 - Row,
    ColIndex is Col - 1,
    nth0(RowIndex, Board, Line),
    nth0(ColIndex, Line, [_, Height]),
    NewHeight is Height + 1,
    replace(ColIndex, [Color, NewHeight], Line, NewLine),
    replace(RowIndex, NewLine, Board, NewBoard).


/*
* Places a block on the board at the specified row and column with a given color and validity:
* put_block(+Board, +Row, +Col, +Color, -NewBoard, +Direction)
* Board: The current game board represented as a list of lists.
* Row: The row number (1-based) where the block should be placed.
* Col: The column number (1-based) where the block should be placed.
* Color: The color of the block to be placed (e.g., 'white', 'black', 'white_valid', 'black_valid', 'white_invalid', 'black_invalid').
* NewBoard: The updated board after placing the block at the given position.
* Direction: The direction of placement (1 or 2), determining the specific function used to place the block.
*
* This predicate chooses the appropriate function to place a block based on the color and direction, 
* and updates the board with the new block at the specified position.
*/
put_block(Board, Row, Col, 'white', NewBoard, 1) :- put_white_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white', NewBoard, 2) :- put_white_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black', NewBoard, 1) :- put_black_block_direction_1(Board, Row, Col, NewBoard). 
put_block(Board, Row, Col, 'black', NewBoard, 2) :- put_black_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white_valid', NewBoard, 1) :- put_white_valid_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white_valid', NewBoard, 2) :- put_white_valid_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black_valid', NewBoard, 1) :- put_black_valid_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black_valid', NewBoard, 2) :- put_black_valid_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white_invalid', NewBoard, 1) :- put_white_invalid_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'white_invalid', NewBoard, 2) :- put_white_invalid_block_direction_2(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black_invalid', NewBoard, 1) :- put_black_invalid_block_direction_1(Board, Row, Col, NewBoard).
put_block(Board, Row, Col, 'black_invalid', NewBoard, 2) :- put_black_invalid_block_direction_2(Board, Row, Col, NewBoard).


/*
* The following predicates handle the placement of blocks (white, black, valid, and invalid)
* on the game board at the specified row and column, using two possible directions. 
* The predicates use the `put_piece/5` predicate to place pieces on the board in specific configurations.
* Each predicate receives the current board, the row and column to place the piece, and returns an updated board.
* The directions determine how the pieces (blocks) interact with the other blocks already placed on the board.
* 
* Direction 1 and 2 define the different directions in which blocks can be placed on the board:
*   - Direction 1: Places pieces in a specific pattern, modifying neighboring cells accordingly.
*   - Direction 2: Places pieces in an alternate pattern, interacting with cells in a different way.
* 
* The predicates consider both "valid" and "invalid" placements of the blocks, ensuring that
* only legal moves are made on the game board.
*/

put_white_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'white', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'black', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black', NewBoard).

put_white_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'black', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'white', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white', NewBoard).
    
put_black_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'black', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'white', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white', NewBoard).

put_black_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'white', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'black', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black', NewBoard).

put_white_valid_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white_valid', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'white_valid', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'black_valid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black_valid', NewBoard).

put_white_valid_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black_valid', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'black_valid', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'white_valid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white_valid', NewBoard).
    
put_black_valid_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black_valid', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'black_valid', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'white_valid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white_valid', NewBoard).

put_black_valid_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white_valid', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'white_valid', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'black_valid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black_valid', NewBoard).

put_white_invalid_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white_invalid', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'white_invalid', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'black_invalid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black_invalid', NewBoard).

put_white_invalid_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black_invalid', TempBoard1),
    put_piece(TempBoard1, Row2, Col, 'black_invalid', TempBoard2),
    put_piece(TempBoard2, Row, Col2, 'white_invalid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white_invalid', NewBoard).
    
put_black_invalid_block_direction_1(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'black_invalid', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'black_invalid', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'white_invalid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'white_invalid', NewBoard).

put_black_invalid_block_direction_2(Board, Row, Col, NewBoard) :-
    Row2 is Row + 1,
    Col2 is Col + 1,
    put_piece(Board, Row, Col, 'white_invalid', TempBoard1),
    put_piece(TempBoard1, Row, Col2, 'white_invalid', TempBoard2),
    put_piece(TempBoard2, Row2, Col, 'black_invalid', TempBoard3),
    put_piece(TempBoard3, Row2, Col2, 'black_invalid', NewBoard).