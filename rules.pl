% By Ruben Vereecken & Ruben Mennes

% VECTOR OF ATTACK
% A chain of 3 rules (see 'battleShip'):
%  1. Generate all valid fields according to the rules of battleship (see 'checkField')
%  2. Check row and column counts (see 'checkAllCounts')
%  3. Verify that all boats are of valid length, as specified (see 'checkAllBoatLengths')

% Working assumptions:
% Matrices look as follows, with ij being element (i, j)
% [[00, 01, 02,.. 0n], [10, 11, 12,.. 1n],.. [n0, n1, n2,.. nn]]

%%%%%%%%%%%
% UTILITY %
%%%%%%%%%%%

% Array[n]
getNthElement(0, [Head|_], Head) :- !.
getNthElement(N, [_|Tail], X) :- N1 is N-1, getNthElement(N1, Tail, X).

% Matrix[x][y]
getXYElement(X, 0, [Head|_], E) :- getNthElement(X, Head, E), !.
getXYElement(X, Y, [_|Tail], E) :- Y1 is Y-1, getXYElement(X, Y1, Tail, E).

% Get nth column of a matrix as an array
getColumnAsArray(Field, X, Array) :- !, getColumnAsArrayHelp(Field, X, 0, Array), !.
getColumnAsArrayHelp(Field, _, Y, []) :- height(Field, Y), !.
getColumnAsArrayHelp(Field, X, Y, [E|Tail]) :- YInc is Y + 1, getXYElement(X, Y, Field, E), getColumnAsArrayHelp(Field, X, YInc, Tail).

width([Head|_], X) :- !, length(Head, X).
height(Field, X) :- !, length(Field, X).

% Create a row of length n filled with specified value
makeRowOfLength(0, _, []) :- !.
makeRowOfLength(N, P, [P|Tail]) :- N1 is N-1, makeRowOfLength(N1, P, Tail).

% Pretty print field
printField([]) :- !.
printField([FirstRow|OtherRows]) :- !, printRow(FirstRow), printField(OtherRows).
printRow([]) :- print('\n'), !.
printRow([RowHead|RowTail]) :- !, print(RowHead), print(' '), printRow(RowTail).

% Utility function that surrounds a field with water for easier rule checks
omringRow(Row, P, [P|NewRow]) :- append(Row, [P], NewRow), !.
omringEveryRow([],_, []) :- !.
omringEveryRow([Row|Tail],P, [Result|ResultTail]) :- !, omringRow(Row, P, Result), omringEveryRow(Tail, P, ResultTail).
omringTable(Table, P, [R2|ResultTail]) :- 
	!, omringEveryRow(Table, P, [R1Head|R1Tail]), length(R1Head, L), makeRowOfLength(L, P, R2), append([R1Head|R1Tail], [R2], ResultTail),!.

%%%%%%%%%
% RULES %
%%%%%%%%%

% Verify all side counts, delegate to checkAllRowCounts and checkAllColumnCounts
checkAllCounts(Field, RowCounts, ColumnCounts) :- 
    !, width(Field, Width), height(Field, Height), checkAllRowCounts(Field, RowCounts), !, checkAllColumnCounts(Field, ColumnCounts, Width, Height), !.

% For every row, verify the boat count
checkAllRowCounts([], []) :- !.
checkAllRowCounts([FirstRow|OtherRows], [FirstCount|OtherCounts]) :-
    checkRowCount(FirstRow, FirstCount), checkAllRowCounts(OtherRows, OtherCounts).

% After checking a whole array of boats, the boat count should be 0
checkRowCount([], 0) :- !.
checkRowCount([StripHead|StripTail], BoatCount) :- 
    boat(StripHead), NextCount is BoatCount - 1, checkRowCount(StripTail, NextCount);
    water(StripHead), checkRowCount(StripTail, BoatCount).
   
% For every column of a field, verify boat count
checkAllColumnCounts(Field, ColumnCounts, Width, Height) :-
    loopCheckX(Field, ColumnCounts, 0, Width, Height).

% Basically do a nested for loop, make sure the counts match
loopCheckX(_, _, X, X, _) :- !. % X < width
loopCheckX(Field, [FirstCount|OtherCounts], X, Width, Height) :-
    loopCheckY(Field, FirstCount, X, 0, Height), XInc is X + 1, loopCheckX(Field, OtherCounts, XInc, Width, Height).

loopCheckY(_, 0, _, Y, Y) :- !. % Y < height
loopCheckY(Field, Count, X, Y, Height) :- Count > 0,
    getXYElement(X, Y, Field, Cell), boat(Cell), YInc is Y + 1, CountDec is Count - 1, loopCheckY(Field, CountDec, X, YInc, Height).
loopCheckY(Field, Count, X, Y, Height) :-
    getXYElement(X, Y, Field, Cell), water(Cell), YInc is Y + 1, loopCheckY(Field, Count, X, YInc, Height).

% Verify boat lengths
% Simply start counting whenever you find the start of a boat (w if we're checking horizontally, n if vertically)
% until you find the end of a boat (w and s respectively). The counted length should then be in the given array
% of allowed lengths. Also does a boat integrity check in the meanwhile.
checkAllBoatLengths(Field, Allowed) :- loopBoatRowCheck(Field, Allowed), !, loopBoatColumnCheck(Field, 0, Allowed), !.

busyOnHorizontalBoat([e|BoatTail], Count, Allowed) :- CountInc is Count + 1, member(CountInc, Allowed), checkBoatRow(BoatTail, Allowed).
busyOnHorizontalBoat([x|BoatTail], Count, Allowed) :- CountInc is Count + 1, busyOnHorizontalBoat(BoatTail, CountInc, Allowed).
   
checkBoatRow([], _) :- !.
checkBoatRow([o|RowTail], Allowed) :- member(1, Allowed), checkBoatRow(RowTail, Allowed).
checkBoatRow([w|RowTail], Allowed) :- busyOnHorizontalBoat(RowTail, 1, Allowed).
checkBoatRow([n|RowTail], Allowed) :- checkBoatRow(RowTail, Allowed).
checkBoatRow([s|RowTail], Allowed) :- checkBoatRow(RowTail, Allowed).
checkBoatRow([x|RowTail], Allowed) :- checkBoatRow(RowTail, Allowed).
checkBoatRow(['~'|RowTail], Allowed) :- checkBoatRow(RowTail, Allowed).

busyOnVerticalBoat([s|BoatTail], Count, Allowed) :- CountInc is Count + 1, member(CountInc, Allowed), checkBoatColumn(BoatTail, Allowed).
busyOnVerticalBoat([x|BoatTail], Count, Allowed) :- CountInc is Count + 1, busyOnVerticalBoat(BoatTail, CountInc, Allowed).
   
checkBoatColumn([], _) :- !.
checkBoatColumn([o|ColumnTail], Allowed) :- member(1, Allowed), checkBoatColumn(ColumnTail, Allowed).
checkBoatColumn([n|ColumnTail], Allowed) :- busyOnVerticalBoat(ColumnTail, 1, Allowed).
checkBoatColumn([w|ColumnTail], Allowed) :- checkBoatColumn(ColumnTail, Allowed).
checkBoatColumn([e|ColumnTail], Allowed) :- checkBoatColumn(ColumnTail, Allowed).
checkBoatColumn([x|ColumnTail], Allowed) :- checkBoatColumn(ColumnTail, Allowed).
checkBoatColumn(['~'|ColumnTail], Allowed) :- checkBoatColumn(ColumnTail, Allowed).

loopBoatRowCheck([], _) :- !.
loopBoatRowCheck([FirstRow|OtherRows], Allowed) :- checkBoatRow(FirstRow, Allowed), loopBoatRowCheck(OtherRows, Allowed).

loopBoatColumnCheck(Field, X, _) :- width(Field, X), !.
loopBoatColumnCheck(Field, X, Allowed) :- 
    getColumnAsArray(Field, X, Column), checkBoatColumn(Column, Allowed), XInc is X + 1, loopBoatColumnCheck(Field, XInc, Allowed).
    
%%%%%%%%%%%%%%%%%%%%%
% Check valid field %
%%%%%%%%%%%%%%%%%%%%%

% Surround a field with water. Then, have a 3x3 window that moves from left to right and from top to bottom.
% Every iteration the surroundings of the center of the window will be checked, that is the middle one against the 8
% surrounding cells, against our grammar that specifies which blocks can be positioned next to which other blocks.
% Afterwards do a little extra check for x because it can be part of both a horizontally or vertically positioned boat but never both.
checkField(Field, Field) :- !, omringTable(Field, '~', Omring), !, checkColumn(Omring), checkXColumn(Omring).

checkRow([TopHead,'~'],[RowHead,'~'],[DownHead,'~']) :- north(RowHead,TopHead), south(RowHead,DownHead).
checkRow([TopPrevious,TopHead,TopNext|TopTail], [RowPrevious,RowHead,RowNext|RowTail], [DownPrevious,DownHead,DownNext|DownTail]) :-
    boat(RowHead),
	north(RowHead, TopHead), east(RowHead, RowNext), west(RowHead,RowPrevious),south(RowHead,DownHead),
	diagonal(RowHead,TopPrevious), diagonal(RowHead,TopNext), diagonal(RowHead,DownNext), diagonal(RowHead, DownPrevious), 
	checkRow([TopHead,TopNext|TopTail],[RowHead,RowNext|RowTail],[DownHead,DownNext|DownTail]).
checkRow([_,TopHead,TopNext|TopTail], [_,RowHead,RowNext|RowTail], [_,DownHead,DownNext|DownTail]) :-
    water(RowHead),
    checkRow([TopHead,TopNext|TopTail],[RowHead,RowNext|RowTail],[DownHead,DownNext|DownTail]).

checkColumn([TopHead,RowHead,LastHead]) :- 
	length(RowHead, Length), makeRowOfLength(Length, ~, LastHead), checkRow(TopHead, RowHead, LastHead).
checkColumn([TopHead, RowHead, DownHead|Tail]) :-
	checkRow(TopHead, RowHead, DownHead), checkColumn([RowHead, DownHead|Tail]).

checkXRow([_,'~'],[RowHead,'~'],[_,'~']) :- notMiddlePiece(RowHead).
checkXRow([TopHead,'~'],[x,'~'],[DownHead,'~']) :- boat(TopHead), boat(DownHead).
checkXRow([_,TopHead,TopNext|TopTail], [_,RowHead,RowNext|RowTail], [_,DownHead,DownNext|DownTail]) :-
    notMiddlePiece(RowHead), checkXRow([TopHead,TopNext|TopTail],[RowHead,RowNext|RowTail],[DownHead,DownNext|DownTail]).
checkXRow([_,TopHead,TopNext|TopTail], [RowPrevious,x,RowNext|RowTail], [_,DownHead,DownNext|DownTail]) :-
    water(TopHead), water(DownHead), boat(RowPrevious), boat(RowNext), 
    checkXRow([TopHead,TopNext|TopTail],[x,RowNext|RowTail],[DownHead,DownNext|DownTail]).
checkXRow([_,TopHead,TopNext|TopTail], [RowPrevious,x,RowNext|RowTail], [_,DownHead,DownNext|DownTail]) :-
    boat(TopHead), boat(DownHead), water(RowPrevious), water(RowNext),
    checkXRow([TopHead,TopNext|TopTail],[x,RowNext|RowTail],[DownHead,DownNext|DownTail]).
	
checkXColumn([TopHead,RowHead,LastHead]) :- 
    length(RowHead, Length), makeRowOfLength(Length, '~', LastHead), checkXRow(TopHead, RowHead, LastHead).
checkXColumn([TopHead, RowHead, DownHead|Tail]) :- 
    checkXRow(TopHead, RowHead, DownHead), checkXColumn([RowHead, DownHead|Tail]).

% battleShip is the function that should be called from outside.
battleShip(Field, XShips, YShips, Ships, Result) :- write('Please allow for a minute.'), nl, checkField(Field, Result), checkAllCounts(Result, YShips, XShips), checkAllBoatLengths(Result, Ships), printField(Result), !.

