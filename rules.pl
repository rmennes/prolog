%%%%%%%%%%%
% UTILITY %
%%%%%%%%%%%

getNthElement(0, [Head|_], Head).
getNthElement(N, [_|Tail], X) :- N1 is N-1, getNthElement(N1, Tail, X).

getXYElement(X, 0, [Head|_], E) :- getNthElement(X, Head, E).
getXYElement(X, Y, [_|Tail], E) :- Y1 is Y-1, getXYElement(X, Y1, Tail, E).

getColumnAsArrayHelp(Field, _, Y, []) :- actualHeight(Field, Y).
getColumnAsArrayHelp(Field, X, Y, [E|Tail]) :- YInc is Y + 1, getXYElement(X, Y, Field, E), getColumnAsArrayHelp(Field, X, YInc, Tail).
getColumnAsArray(Field, X, Array) :- getColumnAsArrayHelp(Field, X, 0, Array).

%Default rules
%%Row and Coumns
width([Head|_], X) :- length(Head, X1), X is X1-1. %width
height(Field, X) :- length(Field, X1), X is X1-1. %height

actualWidth([Head|_], X) :- length(Head, X).
actualHeight(Field, X) :- length(Field, X).

allZero([]).
allZero([0|T]) :- allZero(T).

printRow([]) :- print('\n').
printRow([RowHead|RowTail]) :- print(RowHead), print(' '), printRow(RowTail).
printField([]).
printField([FirstRow|OtherRows]) :- printRow(FirstRow), printField(OtherRows).

%%Removing Duplicates
	memberOf(X,[X|_]).
	memberOf(X,[_|T]) :- memberOf(X,T).

	not(A) :- \+ call(A).

	set([],[]).
	set([H|T],[H|Out]) :-
	    not(memberOf(H,T)),
 	   set(T,Out).
	set([H|T],Out) :-
    		memberOf(H,T),
    		set(T,Out).

%%%%%%%%%
% RULES %
%%%%%%%%%

% Verify side counts

checkRowCount([], 0).
checkRowCount([StripHead|StripTail], BoatCount) :- 
    boat(StripHead), NextCount is BoatCount - 1, checkRowCount(StripTail, NextCount);
    water(StripHead), checkRowCount(StripTail, BoatCount).
    
checkAllRowCounts([], []).
checkAllRowCounts([FirstRow|OtherRows], [FirstCount|OtherCounts]) :-
    checkRowCount(FirstRow, FirstCount), checkAllRowCounts(OtherRows, OtherCounts).
    
loopCheckY(_, 0, _, 0).  % Loop until x=0 && count=0
loopCheckY(Field, Count, X, Y) :- Count > 0, Y > 0,
    XDec is X - 1, YDec is Y - 1, CountDec is Count - 1, getXYElement(XDec, YDec, Field, Cell), boat(Cell), loopCheckY(Field, CountDec, X, YDec);
    XDec is X - 1, YDec is Y - 1, getXYElement(XDec, YDec, Field, Cell), water(Cell), loopCheckY(Field, Count, X, YDec).
   
  
loopCheckX(Field, _, XInc) :- X is XInc - 1, actualWidth(Field, X).
loopCheckX(Field, [FirstCount|OtherCounts], X) :-
    actualHeight(Field, Height), loopCheckY(Field, FirstCount, X, Height), XInc is X + 1, loopCheckX(Field, OtherCounts, XInc).
    
checkAllColumnCounts(Field, ColumnCounts) :-
    loopCheckX(Field, ColumnCounts, 1).
    
checkAllCounts(Field, RowCounts, ColumnCounts, Field) :- checkAllRowCounts(Field, RowCounts), checkAllColumnCounts(Field, ColumnCounts).


% Verify amount of boats
busyOnHorizontalBoat([e|BoatTail], Count, Allowed) :- CountInc is Count + 1, member(CountInc, Allowed), checkBoatRow(BoatTail, Allowed).
busyOnHorizontalBoat([x|BoatTail], Count, Allowed) :- CountInc is Count + 1, busyOnHorizontalBoat(BoatTail, CountInc, Allowed).
   
checkBoatRow([], _).
checkBoatRow([o|RowTail], Allowed) :- member(1, Allowed), checkBoatRow(RowTail, Allowed).
checkBoatRow([w|RowTail], Allowed) :- busyOnHorizontalBoat(RowTail, 1, Allowed).
checkBoatRow([n|RowTail], Allowed) :- checkBoatRow(RowTail, Allowed).
checkBoatRow([s|RowTail], Allowed) :- checkBoatRow(RowTail, Allowed).
checkBoatRow([x|RowTail], Allowed) :- checkBoatRow(RowTail, Allowed).
checkBoatRow(['~'|RowTail], Allowed) :- checkBoatRow(RowTail, Allowed).

busyOnVerticalBoat([s|BoatTail], Count, Allowed) :- CountInc is Count + 1, member(CountInc, Allowed), checkBoatColumn(BoatTail, Allowed).
busyOnVerticalBoat([x|BoatTail], Count, Allowed) :- CountInc is Count + 1, busyOnVerticalBoat(BoatTail, CountInc, Allowed).
   
checkBoatColumn([], _).
checkBoatColumn([o|ColumnTail], Allowed) :- member(1, Allowed), checkBoatColumn(ColumnTail, Allowed).
checkBoatColumn([n|ColumnTail], Allowed) :- busyOnVerticalBoat(ColumnTail, 1, Allowed).
checkBoatColumn([w|ColumnTail], Allowed) :- checkBoatColumn(ColumnTail, Allowed).
checkBoatColumn([e|ColumnTail], Allowed) :- checkBoatColumn(ColumnTail, Allowed).
checkBoatColumn([x|ColumnTail], Allowed) :- checkBoatColumn(ColumnTail, Allowed).
checkBoatColumn(['~'|ColumnTail], Allowed) :- checkBoatColumn(ColumnTail, Allowed).

loopBoatRowCheck([], _).
loopBoatRowCheck([FirstRow|OtherRows], Allowed) :- checkBoatRow(FirstRow, Allowed), loopBoatRowCheck(OtherRows, Allowed).

loopBoatColumnCheck(Field, X, _) :- actualWidth(Field, X).
loopBoatColumnCheck(Field, X, Allowed) :- 
    getColumnAsArray(Field, X, Column), checkBoatColumn(Column, Allowed), XInc is X + 1, loopBoatColumnCheck(Field, XInc, Allowed).
    
checkAmountOfBoats(Field, Allowed) :- loopBoatRowCheck(Field, Allowed), loopBoatColumnCheck(Field, 0, Allowed).
    

%CheckPosition
%%(0,0)
checkPosition(0, 0, _, P) :- leftTopCorner(P).

%%(0, Y) met 0 < Y < YMax
checkPosition(0, Y, Field, P) :- width(Field, YMax), 0 < Y, Y < YMax, leftBorder(P).

%%(0, Y) met Y == YMax
checkPosition(0, Y, Field, P) :- width(Field, Y), leftButtomCorner(P).

%%(X, 0) met 0 < X < XMax
checkPosition(X, 0, Field, P) :- height(Field, XMax), 0 < X, X < XMax, topBorder(P).

%%(X, Y) met 0 < X < XMax && 0 < Y < YMax
checkPosition(X, Y, Field, _) :- height(Field, XMax), width(Field, YMax), 0 < X, X < XMax, 0 < Y, Y < YMax.

%%(X, Y) met 0 < X < XMax && Y == YMax
checkPosition(X, Y, Field, P) :- height(Field, XMax), width(Field, Y), 0 < X, X < XMax, buttomBorder(P).

%%(X, 0) met X == XMax
checkPosition(X, 0, Field, P) :- height(Field, X), rightTopCorner(P).

%%(X, Y) met X == XMax && 0 < Y < YMax
checkPosition(X, Y, Field, P) :- height(Field, X), width(Field, YMax), 0 < Y, Y < YMax, rightBorder(P).

%%(X, Y) met X == XMax && Y == YMax
checkPosition(X, Y, Field, P) :- height(Field, X), width(Field, Y), rightButtomCorner(P).

%CheckAround
%%(0,0)
checkAround(0, 0, Field) :-
	getXYElement(0, 0, Field, E), checkPosition(0, 0, Field, E),
	getXYElement(0, 1, Field, Down), checkPosition(0, 1, Field, Down),	
	getXYElement(1, 0, Field, Right), checkPosition(1, 0, Field, Right),
	getXYElement(1, 1, Field, RightDown),checkPosition(1, 1, Field, RightDown),
	south(E, Down), 
	east(E, Right),
	diagonal(E, RightDown).

%%(0, Y) met 0 < Y < width
checkAround(0, Y, Field) :- width(Field, Rows), Y > 0, Y < Rows, YS is Y-1, YA is Y+1,
	getXYElement(0, Y, Field, E), checkPosition(0, Y, Field, E),
	getXYElement(0, YS, Field, Up), checkPosition(0, YS, Field, Up),
	getXYElement(0, YA, Field, Down), checkPosition(0, YA, Field, Down),
	getXYElement(1, YS, Field, RightUp), checkPosition(1, YS, Field, RightUp), 
	getXYElement(1, Y, Field, Right), checkPosition(1, Y, Field, Right),
	getXYElement(1, YA, Field, RightDown),checkPosition(1, YA, Field, RightDown),
	north(E, Up),south(E, Down),east(E, Right), diagonal(E, RightUp), diagonal(E, RightDown).

%%(0, Y) met Y == width
checkAround(0, Y, Field) :- width(Field, Y), YS is Y-1,
	getXYElement(0, Y, Field, E),checkPosition(0, Y, Field, E),
	getXYElement(1, Y, Field, Right),checkPosition(1, Y, Field, Right),
	getXYElement(0, YS, Field, Up),checkPosition(0, YS, Field, Up),
	getXYElement(1, YS, Field, RightUp),checkPosition(1, YS, Field, RightUp),
	north(E, Up), east(E, Right), diagonal(E, RightUp).


%%(X, 0) met 0 < X < height
checkAround(X, 0, Field) :- width(Field, Width), X > 0, X < Width, XS is X-1, XA is X+1,
	getXYElement(X, 0, Field, E), checkPosition(X, 0, Field, E),
	getXYElement(XS, 0, Field, Left), checkPosition(XS, 0, Field, Left),
	getXYElement(XA, 0, Field, Right), checkPosition(XA, 0, Field, Right),
	getXYElement(XS, 1, Field, LeftDown), checkPosition(XS, 1, Field, LeftDown),
	getXYElement(X, 1, Field, Down), checkPosition(X, 1, Field, Down),
	getXYElement(XA, 1, Field, RightDown),checkPosition(XA, 1, Field, RightDown),
	west(E, Left), south(E, Down), east(E, Right), diagonal(E, LeftDown), diagonal(E, RightDown).

%%(X, 0) met X == Width
checkAround(X, 0, Field) :- width(Field, X), XS is X-1,
	getXYElement(X, 0, Field, E),checkPosition(X, 0, Field, E),
	getXYElement(X, 1, Field, Down),checkPosition(X, 1, Field, Down),
	getXYElement(XS, 0, Field, Left),checkPosition(XS, 0, Field, Left), 
	getXYElement(XS, 1, Field, LeftDown),checkPosition(XS, 1, Field, LeftDown),
	south(E, Down), west(E, Left), diagonal(E, LeftDown).

%%(X, Y) met 0 < X < height && 0 < Y < width
checkAround(X, Y, Field) :- width(Field, Rows), height(Field, Columns), X > 0, Y > 0, X < Rows, Y < Columns, XS is X-1, YS is Y-1, XA is X+1, YA is Y+1, 
	getXYElement(X, Y, Field, E),checkPosition(X, Y, Field, E),
	getXYElement(XS, YS, Field, LeftUp), checkPosition(XS, YS, Field, LeftUp),
	getXYElement(XS, Y, Field, Left), checkPosition(XS, Y, Field, Left),
	getXYElement(XS, YA, Field, LeftDown), checkPosition(XS, YA, Field, LeftDown),
	getXYElement(X, YS, Field, Up), checkPosition(X, YS, Field, Up),
	getXYElement(X, YA, Field, Down), checkPosition(X, YA, Field, Down),
	getXYElement(XA, YS, Field, RightUp), checkPosition(XA, YS, Field, RightUp),
	getXYElement(XA, Y, Field, Right), checkPosition(XA, Y, Field, Right),
	getXYElement(XA, YA, Field, RightDown),checkPosition(XA, YA, Field, RightDown),
	north(E, Up), east(E, Left), south(E, Down), west(E, Right), diagonal(E, LeftUp), diagonal(E, RightUp), diagonal(E, LeftDown), diagonal(E, RightDown).

%%(X, Y) met X = height && 0 < Y < width
checkAround(X, Y, Field) :- height(Field, Height), width(Field, X), Y > 0, Y < Height, XS is X-1, YS is Y-1, YA is Y+1,
	getXYElement(X, Y, Field, E),checkPosition(X, Y, Field, E), 
	getXYElement(X, YS, Field, Up),checkPosition(X, YS, Field, Up),
	getXYElement(X, YA, Field, Down),checkPosition(X, YA, Field, Down),
	getXYElement(XS, YS, Field, LeftUp), checkPosition(XS, YS, Field, LeftUp),
	getXYElement(XS, Y, Field, Left),checkPosition(XS, Y, Field, Left),
	getXYElement(XS, YA, Field, LeftDown),checkPosition(XS, YA, Field, LeftDown),
	north(E, Up), south(E, Down), west(E, Left), diagonal(E, LeftUp), diagonal(E, LeftDown).

%%(X, Y) met 0 < X < height && Y = width
checkAround(X, Y, Field) :- height(Field, Y), width(Field, Width), X > 0, X < Width, XS is X-1, XA is X+1, YS is Y-1,
	getXYElement(X, Y, Field, E),checkPosition(X, Y, Field, E),
	getXYElement(XS, YS, Field, LeftUp),checkPosition(XS, YS, Field, LeftUp),
	getXYElement(XS, Y, Field, Left), checkPosition(XS, Y, Field, Left),
	getXYElement(X, YS, Field, Up),checkPosition(X, YS, Field, Up),
	getXYElement(XA, Y, Field, Right),checkPosition(XA, Y, Field, Right),
	getXYElement(XA, YS, Field, RightUp),checkPosition(XA, YS, Field, RightUp),
	north(E, Up), west(E, Left), east(E, Right), diagonal(E, LeftUp), diagonal(E, RightUp).

%%(X, Y) met X = heigth && Y = width
checkAround(X, Y, Field) :- height(Field, Y),width(Field, X), XS is X-1, YS is Y-1,
	getXYElement(X, Y, Field, E),checkPosition(X, Y, Field, E),
	getXYElement(XS, YS, Field, LeftUp),checkPosition(XS, YS, Field, LeftUp),
	getXYElement(XS, Y, Field, Left),checkPosition(XS, Y, Field, Left),
	getXYElement(X, YS, Field, Up),checkPosition(X, YS, Field, Up),
	north(E, Up), west(E, Left), diagonal(E, LeftUp).

%% 0 <= Y <= height
%% 0 <= X <= width

makeRow([TopHead,'~'],[RowHead,'~'],[DownHead,'~'],[]) :-
	north(RowHead,TopHead), south(RowHead,DownHead).
makeRow([TopPrevious,TopHead,TopNext|TopTail], [RowPrevious,RowHead,RowNext|RowTail], [DownPrevious,DownHead,DownNext|DownTail], [RowHead|ResultTail]) :-
	north(RowHead, TopHead), east(RowHead, RowNext), west(RowHead,RowPrevious),south(RowHead,DownHead),
	diagonal(RowHead,TopPrevious), diagonal(RowHead,TopNext), diagonal(RowHead,DownNext), diagonal(RowHead, DownPrevious), 
	makeRow([TopHead,TopNext|TopTail],[RowHead,RowNext|RowTail],[DownHead,DownNext|DownTail], ResultTail).

checkField(0, Y, Field) :- height(Field, YMax),  Y > YMax.
checkField(X, Y, Field) :- width(Field, X), checkAround(X, Y, Field), YNext is Y+1, checkField(0, YNext, Field).
checkField(X, Y, Field) :- width(Field, XMax), X < XMax, checkAround(X, Y, Field), XNext is X+1, checkField(XNext, Y, Field).

makeField(Field, Field) :- checkField(0, 0, Field).
