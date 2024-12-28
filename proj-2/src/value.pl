:- consult(utils).

% Main predicate to initiate search from multiple starting points
get_value(Board, Player, MaxValue) :-
    findall(Value, (between(0, 9, Row), between(0, 9, Col), 
                    get_value_aux(Board, Row, Col, [], Player, 0, Value)), Values),
    max_list(Values, MaxValue).

% White recursive case: Explore all 4 directions
get_value_aux(Board, Row, Col, Visited, 'white', CurrentValue, FinalValue) :-
    Row >= 0, Row < 10, Col >= 0, Col < 10,
    % Check if the current cell has already been visited
    \+ member([Row, Col], Visited),
    
    % Mark the current cell as visited
    NewVisited = [[Row, Col] | Visited],
    
    % Get the color of the current cell
    nth0(Row, Board, Line),
    nth0(Col, Line, ['white', _]),

    % Calculate new indices and ensure they are evaluated as integers
    NewRow1 is Row + 1,
    NewRow2 is Row - 1,
    NewCol1 is Col + 1,
    NewCol2 is Col - 1,
    
    % Recursively explore all four directions (right, left, down, up)
    (get_value_aux(Board, Row, NewCol1, NewVisited, 'white', CurrentValue, FinalValue) ; % Move right
    get_value_aux(Board, Row, NewCol2, NewVisited, 'white', CurrentValue, FinalValue) ; % Move left
    (NewValue is CurrentValue - 1, get_value_aux(Board, NewRow1, Col, NewVisited, 'white', NewValue, FinalValue)) ; % Move down
    (NewValue is CurrentValue + 1, get_value_aux(Board, NewRow2, Col, NewVisited, 'white', NewValue, FinalValue))). % Move up

% Black recursive case: Explore all 4 directions
get_value_aux(Board, Row, Col, Visited, 'black', CurrentValue, FinalValue) :-
    Row >= 0, Row < 10, Col >= 0, Col < 10,
    % Check if the current cell has already been visited
    \+ member([Row, Col], Visited),
    
    % Mark the current cell as visited
    NewVisited = [[Row, Col] | Visited],
    
    % Get the color of the current cell
    nth0(Row, Board, Line),
    nth0(Col, Line, ['black', _]),

    % Calculate new indices and ensure they are evaluated as integers
    NewRow1 is Row + 1,
    NewRow2 is Row - 1,
    NewCol1 is Col + 1,
    NewCol2 is Col - 1,
    
    % Recursively explore all four directions (right, left, down, up)
    ((NewValue is CurrentValue + 1, get_value_aux(Board, Row, NewCol1, NewVisited, 'black', NewValue, FinalValue)) ; % Move right
    (NewValue is CurrentValue - 1, get_value_aux(Board, Row, NewCol2, NewVisited, 'black', NewValue, FinalValue)) ; % Move left
    get_value_aux(Board, NewRow1, Col, NewVisited, 'black', CurrentValue, FinalValue) ; % Move down
    get_value_aux(Board, NewRow2, Col, NewVisited, 'black', CurrentValue, FinalValue)). % Move up

% Base case: stop recursion when a cell is out of bounds or already visited
get_value_aux(_, Row, Col, _, _, CurrentValue, FinalValue) :-
    (Row < 0; Row > 9; Col < 0; Col > 9),
    !,
    FinalValue = CurrentValue.

% Base case: stop recursion when a cell was already visited
get_value_aux(_, Row, Col, Visited, _, CurrentValue, FinalValue) :-
    member([Row, Col], Visited),
    FinalValue = CurrentValue.

% Base case: stop recursion when the cell does not match the player
get_value_aux(Board, Row, Col, _, Player, CurrentValue, FinalValue) :-
    Row >= 0, Row < 10, Col >= 0, Col < 10,
    nth0(Row, Board, Line),
    nth0(Col, Line, [Color, _]),
    Color \= Player,
    FinalValue = CurrentValue.