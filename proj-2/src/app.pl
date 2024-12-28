:-use_module(library(file_systems)).

/*
* Retrieves the file path for a specific menu type:
* menu_path(+MenuType, -Path)
* MenuType: The type of menu (e.g., main, instructions, human_bot, bot_bot, size).
* Path: The file path corresponding to the menu type.
*/
menu_path(main, Path):-         Path = './menus/main_menu.txt'. 
menu_path(instructions, Path):- Path = './menus/instructions.txt'. 
menu_path(human_bot, Path):-    Path = './menus/human_bot_menu.txt'. 
menu_path(bot_bot, Path):-      Path = './menus/computer_computer.txt'. 
menu_path(size, Path):-         Path = './menus/board_size_menu.txt'.


% Displays the main menu
main:- 
    display_menu(main),
    repeat,
    read_digit_between(1,5,Input),
    read_specific_char('\n'),
    change_menu(Input, main).


% Displays the instructions
instructions:- 
    display_menu(instructions),
    repeat,
    peek_code(_), skip_line,
    change_menu(_, instructions).


% Displays the human vs computer menu
human_bot:-
    display_menu(human_bot),
    repeat,
    read_digit_between(1, 3, Input),
    read_aux(Input, Res),
    change_menu(Res, human_bot).

% Displays the computer vs computer menu
bot_bot:-
    display_menu(bot_bot),
    repeat,
    read_digit_between(1, 3, Input),
    read_aux(Input, Res),
    change_menu(Res, bot_bot).

/*
* Handles reading user input based on expected formats:
* read_aux(+ExpectedValue, -Result)
* Result: The parsed result, which can either be a single value or a pair (Value1-Value2), depending on the input format.
*/
read_aux(3, 3):- read_specific_char('\n').
read_aux(V1, V1-V2):- 
    read_specific_char('-'),
    read_digit_between(1, 2, V2),
    read_specific_char('\n').

/*
* Displays the board size menu and handles user input:
* size(+Mode, +Back)
* Mode: The current game mode, passed to the start predicate after validation.
* Back: Information used to return to the previous menu or state.
* Prompts the user to select a board size. Currently, 4x4 and 6x6 are disabled, and if an invalid input is written an error message is shown, and the input process repeats.
*/
size(Mode, Back):-
    display_menu(size),
    repeat,
    read_digit_between(1, 4, Input),
    read_specific_char('\n'),
    (
        Input == 4; 
        Input == 2
    ) ->
    start(Mode, Input, Back);
    writeln('Invalid value. Please enter 4 or 2.'), 
    fail.

% Closes the program
exit:- clear_console.

/*
* Displays the menu with the given name:
* display_menu(+Menu)
* Menu: The name of the menu to display (e.g., main, instructions, size).
* Clears the console, retrieves the file path for the specified menu, 
* and reads and displays the content from the file.
*/
display_menu(Menu):-
    clear_console,
    menu_path(Menu, Path),
    read_from_file(Path).

/*
* Changes the menu according to the given value:
* change_menu(+Input, +CurrentMenu)
* Input: The user's input that determines which menu to display next (e.g., 1-5 for main menu, Level-1 or Level-2 for other game modes).
* CurrentMenu: The current menu being displayed (e.g., main, human_bot, bot_bot, instructions).
* Based on the value entered, the appropriate action is taken, either changing to another menu or performing an action related to game mode selection.
*/
change_menu(1, main):- size(['player-player',0], main).
change_menu(2, main):- human_bot.
change_menu(3, main):- bot_bot.
change_menu(4, main):- instructions.
change_menu(5, main):- exit.

% Human_bot game mode
change_menu(3, human_bot):- main.

change_menu(Difficulty-1, human_bot):-  
    size(['computer-player',Difficulty], human_bot).

change_menu(Difficulty-2, human_bot):-  
    size(['player-computer',Difficulty], human_bot).

change_menu(3, bot_bot):- main.

change_menu(Level1-Level2, bot_bot):-  
    size(['computer-computer', Level1, Level2], bot_bot).

change_menu(_, instructions):- main.

/*
* Starts the game with the given mode and size:
* start(+Mode, +Input, +Back)
* Mode: The current game mode (e.g., player-player, computer-player).
* Input: The chosen option of the user. 
* Size: Board size, currently 5x5 is the only one that works.
* Back: Information used to navigate back to the previous menu, which is not used in this case.
* The game is started with the given mode and size. If the option selected is 4, the process returns to the previous state (Back).
*/
start(_, 4, Back):- Back.

start(GameMode, Input, _):- 
    Size is Input + 3,
    game(GameMode, Size),
    prompt_restart_or_menu(GameMode, Size).

/*
* Prompts the user to choose whether to play again or go back to the main menu:
* prompt_restart_or_menu(+Mode, +Size)
* Mode: The current game mode (e.g., player-player, computer-player).
* Size: The current game board size.
* Displays options to the user to either play the game again or return to the main menu. Based on the user's input (1 or 2), the appropriate action is taken by calling the `handle_choice/3` predicate.
*/
prompt_restart_or_menu(GameMode, Size) :-
    write('Do you want to play again or go back to the main menu?'), nl,
    write('1. Play Again'), nl,
    write('2. Go Back to Main Menu'), nl,
    write('Enter your choice: '),
    repeat,
    read_digit_between(1, 2, Input),
    read_specific_char('\n'),
    handle_choice(Input, GameMode, Size).


/*
* Handles the user's choice after prompting to restart or return to the main menu:
* handle_choice(+Choice, +Mode, +Size)
* Choice: The user's input, either 1 (Play Again) or 2 (Go Back to Main Menu).
* Mode: The current game mode (e.g., player-player, computer-player).
* Size: The current game board size.
* Depending on the user's choice:
* - If Choice is 1, the game is restarted with the same mode and size, and the user is prompted again.
* - If Choice is 2, the program returns to the main menu.
*/
handle_choice(1, GameMode, Size) :-
    game(GameMode, Size),
    prompt_restart_or_menu(GameMode, Size).

handle_choice(2, _, _) :-
    main.

/*
* Reads and prints the content of a file:
* read_from_file(+Path)
* Path: The file path to be read and displayed.
* Opens the file specified by Path, reads its content using the `print_file/1` predicate, 
* then closes the file stream, and outputs a newline.
*/
read_from_file(Path):-
    open(Path, read, Stream),
    print_file(Stream),
    close(Stream),
    nl.

/*
* Prints the content of a file:
* print_file(+Stream)
* Stream: The file stream to be read and printed.
* Recursively reads and prints each character from the file stream until the end of the file is reached.
* It handles printing the entire content of the file by reading one character at a time.
*/
print_file(Stream):-
    peek_code(Stream,-1).

print_file(Stream):-
    get_char(Stream, Char),
    write(Char),
    print_file(Stream).

