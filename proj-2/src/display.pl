:- consult(utils).

% ---------------------------------------------
% ------------- Aux Functions -----------------
% ---------------------------------------------

print_grey_line :-
    write_grey('                                    '),
    nl.

print_double_grey_line :-
    write_grey('                                    '),
    write_grey('                                    '),
    nl.

print_white_line :-
    write_white('                                    '),
    nl.

print_black_line :-
    write_black('                                    '),
    nl.

print_white_square(C) :-
    set_color_white,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_black_square(C) :-
    set_color_black,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_white_valid_square(C) :-
    set_color_white_valid,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_black_valid_square(C) :-
    set_color_black_valid,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_white_invalid_square(C) :-
    set_color_white_invalid,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_black_invalid_square(C) :-
    set_color_black_invalid,
    write(' '),
    write(C),
    write(' '),
    reset_color.

print_empty_square(C) :-
    set_color_grey,
    write(' '),
    write(C),
    write(' '),
    reset_color.

left_margin :- write('                  ').

% ---------------------------------------------
% ---------------- Board ----------------------
% ---------------------------------------------

print_board(Board) :-
    nl,
    left_margin,
    print_empty_square(' '),
    set_color_white,
    write('                              '),
    set_color_grey,
    print_empty_square(' '),
    reset_color,
    nl,
    print_board_aux(Board, 0),
    left_margin,
    print_empty_square(' '),
    set_color_white,
    write(' 1  2  3  4  5  6  7  8  9  0 '),
    set_color_grey,
    print_empty_square(' '),
    reset_color,
    nl.

print_board_aux([], _).
print_board_aux([Row | Board], 0) :-
    left_margin,
    print_black_square(0),
    print_board_row(Row),
    print_black_square(' '),
    nl,
    print_board_aux(Board, 9),
    !.
print_board_aux([Row | Board], RowIndex) :-
    left_margin,
    print_black_square(RowIndex),
    print_board_row(Row),
    print_black_square(' '),
    nl,
    NewRowIndex is RowIndex - 1,
    print_board_aux(Board, NewRowIndex),
    !.

print_board_row([]).
print_board_row([['empty', Height] | Row]) :- print_empty_square(Height), print_board_row(Row).
print_board_row([['white', Height] | Row]) :- print_white_square(Height), print_board_row(Row).
print_board_row([['black', Height] | Row]) :- print_black_square(Height), print_board_row(Row).
print_board_row([['white_valid', Height] | Row]) :- print_white_valid_square(Height), print_board_row(Row).
print_board_row([['black_valid', Height] | Row]) :- print_black_valid_square(Height), print_board_row(Row).
print_board_row([['white_invalid', Height] | Row]) :- print_white_invalid_square(Height), print_board_row(Row).
print_board_row([['black_invalid', Height] | Row]) :- print_black_invalid_square(Height), print_board_row(Row).

% ---------------------------------------------
% ------------- Player Turn -------------------
% ---------------------------------------------

print_player_turn('white') :-
    nl,
    left_margin,
    print_white_square(' '),
    write_grey('      White Player Turn!      '),
    print_white_square(' '),
    nl. 
print_player_turn('black') :-
    nl,
    left_margin,
    print_black_square(' '),
    write_grey('      Black Player Turn!      '),
    print_black_square(' '),
    nl.

% ---------------------------------------------
% ------------- Number Blocks -----------------
% ---------------------------------------------

print_header(WhiteBlocks, BlackBlocks) :-
    nl,
    print_double_grey_line,
    write_grey('     --     Blocks  Left     --     '),
    write_grey('     --       Controls       --     '),
    nl,
    print_double_grey_line,
    print_empty_square(' '),
    print_black_square(' '),
    print_white_square(' '),
    ( WhiteBlocks < 10 
        ->  write_grey('     '),
            write_grey(WhiteBlocks),
            write_grey('  White'),
            write_grey('     ')
    ; WhiteBlocks >= 10
        ->  write_grey('     '),
            write_grey(WhiteBlocks),
            write_grey(' White'),
            write_grey('     ')),
    print_white_square(' '),
    print_black_square(' '),
    print_empty_square(' '),
    print_grey_line,
    print_empty_square(' '),
    print_black_square(' '),
    print_white_square(' '),
    ( WhiteBlocks = 1 
        ->  write_grey('      '),
            write_grey('Block '),
            write_grey('      ')
    ; WhiteBlocks \= 1
        ->  write_grey('      '),
            write_grey('Blocks'),
            write_grey('      ')),
    print_white_square(' '),
    print_black_square(' '),
    print_empty_square(' '),
    write_grey('    Move       Rotate     Confirm   '),
    nl,
    print_double_grey_line,
    print_empty_square(' '),
    print_black_square(' '),
    print_black_square(' '),
    ( BlackBlocks < 10 
        ->  write_grey('     '),
            write_grey(BlackBlocks),
            write_grey('  Black'),
            write_grey('     ')
    ; BlackBlocks >= 10 
        ->  write_grey('     '),
            write_grey(BlackBlocks),
            write_grey(' Black'),
            write_grey('     ')),
    print_white_square(' '),
    print_white_square(' '),
    print_empty_square(' '),
        % '     --       Controls       --     '
    % '    Move       Rotate     Confirm   '
    % '                                    '
    % '     !W!        !R!         !C!     '
    % '  !A!!S!!D!                         '
    % 'AWSDRC'
    write_grey('     '),
    print_white_square('W'),
    write_grey('        '),
    print_white_square('R'),
    write_grey('         '),
    print_white_square('C'),
    write_grey('     '),
    nl,
    print_empty_square(' '),
    print_white_square(' '),
    print_white_square(' '),
    ( BlackBlocks = 1 
        ->  write_grey('      '),
            write_grey('Block '),
            write_grey('      ')
    ; BlackBlocks \= 1
        ->  write_grey('      '),
            write_grey('Blocks'),
            write_grey('      ')),
    print_black_square(' '),
    print_black_square(' '),
    print_empty_square(' '),
    write_grey('  '),
    print_white_square('A'),
    print_black_square('S'),
    print_white_square('D'),
    write_grey('                         '),
    nl,
    print_double_grey_line.

% ---------------------------------------------
% ------------- Valid Moves -------------------
% ---------------------------------------------

print_valid_moves(ValidMoves) :-
    left_margin,
    print_grey_line,
    left_margin,
    write_grey('     --     Valid  Moves     --     '),
    nl,
    left_margin,
    print_grey_line,
    print_valid_moves_aux(ValidMoves).


print_valid_moves_aux([]) :-
    left_margin,
    print_grey_line,
    !.
print_valid_moves_aux([[Row1, Col1]]) :-
    left_margin,
    write_grey('    '),
    write_grey(Col1),
    write_grey('-'),
    write_grey(Row1),
    write_grey('                             '),
    nl,
    print_valid_moves_aux([]),
    !.
print_valid_moves_aux([[Row1, Col1], [Row2, Col2]]) :-
    left_margin,
    write_grey('    '),
    write_grey(Col1),
    write_grey('-'),
    write_grey(Row1),
    write_grey(' | '),
    write_grey(Col2),
    write_grey('-'),
    write_grey(Row2),
    write_grey('                       '),
    nl,
    print_valid_moves_aux([]),
    !.
print_valid_moves_aux([[Row1, Col1], [Row2, Col2], [Row3, Col3]]) :-
    left_margin,
    write_grey('    '),
    write_grey(Col1),
    write_grey('-'),
    write_grey(Row1),
    write_grey(' | '),
    write_grey(Col2),
    write_grey('-'),
    write_grey(Row2),
    write_grey(' | '),
    write_grey(Col3),
    write_grey('-'),
    write_grey(Row3),
    write_grey('                 '),
    nl,
    print_valid_moves_aux([]),
    !.
print_valid_moves_aux([[Row1, Col1], [Row2, Col2], [Row3, Col3], [Row4, Col4]]) :-
    left_margin,
    write_grey('    '),
    write_grey(Col1),
    write_grey('-'),
    write_grey(Row1),
    write_grey(' | '),
    write_grey(Col2),
    write_grey('-'),
    write_grey(Row2),
    write_grey(' | '),
    write_grey(Col3),
    write_grey('-'),
    write_grey(Row3),
    write_grey(' | '),
    write_grey(Col4),
    write_grey('-'),
    write_grey(Row4),
    write_grey('           '),
    nl,
    print_valid_moves_aux([]),
    !.
print_valid_moves_aux([[Row1, Col1], [Row2, Col2], [Row3, Col3], [Row4, Col4], [Row5, Col5] | ValidMoves]) :-
    left_margin,
    write_grey('    '),
    write_grey(Col1),
    write_grey('-'),
    write_grey(Row1),
    write_grey(' | '),
    write_grey(Col2),
    write_grey('-'),
    write_grey(Row2),
    write_grey(' | '),
    write_grey(Col3),
    write_grey('-'),
    write_grey(Row3),
    write_grey(' | '),
    write_grey(Col4),
    write_grey('-'),
    write_grey(Row4),
    write_grey(' | '),
    write_grey(Col5),
    write_grey('-'),
    write_grey(Row5),
    write_grey('     '),
    nl,
    print_valid_moves_aux(ValidMoves), 
    !.

print_valid_move_message :-
    left_margin,
    print_black_valid_square(' '),
    write_valid('         Valid Move!          '),
    print_black_valid_square(' '),
    nl.

print_invalid_move_message :-
    left_margin,
    print_black_invalid_square(' '),
    write_invalid('        Invalid Move!         '),
    print_black_invalid_square(' '),
    nl.

print_winner_message('white') :-
    nl,
    left_margin,
    print_white_line,
    left_margin,
    print_grey_line,
    left_margin,
    print_white_square(' '),
    write_grey('       White Player Won!      '),
    print_white_square(' '),
    nl,
    left_margin,
    print_grey_line,
    left_margin,
    print_white_line,
    nl.

print_winner_message('black') :-
    nl,
    left_margin,
    print_black_line,
    left_margin,
    print_grey_line,
    left_margin,
    print_black_square(' '),
    write_grey('       Black Player Won!      '),
    print_black_square(' '),
    nl,
    left_margin,
    print_grey_line,
    left_margin,
    print_black_line,
    nl.