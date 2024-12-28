:- use_module(library(lists)).

% Replaces an element at a given index in a list
replace(Index, Element, List, Result) :-
  nth0(Index, List, _, R),
  nth0(Index, Result, Element, R).

% Finds the maximum element in a list
max_list([X], X).
max_list([X, Y | Rest], Max) :-
    max_list([Y | Rest], MaxRest),
    Max is max(X, MaxRest).

% Reads a line of characters until a newline is encountered
get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1), !.
get_line(Result, Acc):-
    atom_chars(Result, Acc), !.

% Clears the buffer by consuming characters until a newline
clear_buffer:-
    repeat,
    get_char(C),
    C = '\n'.

% Clears the console screen 
clear_console:-
    write('\33\[2J').

% Color setting predicates for the console output
set_color_white :-
    write('\e[47m'),
    write('\e[90m').

set_color_black :-
    write('\e[40m'),
    write('\e[90m').

set_color_white_valid :-
    write('\e[48;5;12m'),
    write('\e[38;5;18m').

set_color_black_valid :-
    write('\e[48;5;18m'),
    write('\e[38;5;12m').

set_color_white_invalid :-
    write('\e[48;5;9m'),
    write('\e[38;5;1m').

set_color_black_invalid :-
    write('\e[48;5;1m'),
    write('\e[38;5;9m').

set_color_grey :-
    write('\e[48;5;239m'),
    write('\e[38;5;250m').

reset_color :-
    write('\e[0m').

% Functions to write text in different colors
write_grey(S) :-
    set_color_grey,
    write(S),
    reset_color.

write_white(S) :-
    set_color_white,
    write(S),
    reset_color.

write_black(S) :-
    set_color_black,
    write(S),
    reset_color.

write_valid(S) :-
    set_color_white_valid,
    write(S),
    reset_color.

write_invalid(S) :-
    set_color_white_invalid,
    write(S),
    reset_color.

read_specific_char(Value):-
    get_char(Char),
    Value == Char, !.
    
% Reads a specific character and checks if it matches the expected one
read_specific_char(_):-
    skip_line,
    write('Invalid input!\n'), fail.
    
% Reads a digit between Min and Max, ensuring it falls in the specified range
read_digit_between(Min, Max, Value):-
    read_digit(X),
    between(Min, Max, X),!,
    Value is X.

read_digit_between(_, _, _):-
    skip_line, 
    write('Invalid input!\n'), fail.

% Reads a single digit and ensures it is valid
read_digit(_):-
    peek_code(Value),
    \+between(48, 57, Value), !, fail.

read_digit(Value):-
    get_code(Ascii),
    char_code('0', AsciiZero),
    Value is Ascii - AsciiZero.