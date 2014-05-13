%%%%%%%%%%%
% UTILITY %
%%%%%%%%%%%

getNthElement(0, [Head|_], Head).
getNthElement(N, [_|Tail], X) :- N1 is N-1, getNthElement(N1, Tail, X).

getXYElement(X, 0, [Head|_], E) :- getNthElement(X, Head, E).
getXYElement(X, Y, [_|Tail], E) :- Y1 is Y-1, getXYElement(X, Y1, Tail, E).

% Width & height of 2-dimensional field
width([Head|_], X) :- length(Head, X). %width
height(Field, X) :- length(Field, X). %height

allZero([]).
allZero([0|T]) :- allZero(T).

%%%%%%%%%
% RULES %
%%%%%%%%%

%Check whether field satisfies the numbers on the side
checkRowCount([], 0).
checkRowCount([StripHead|StripTail], BoatCount) :- 
    boat(StripHead), NextCount is BoatCount - 1, checkRowCount(StripTail, NextCount);
    water(StripHead), checkRowCount(StripTail, BoatCount).
    
checkAllRowCounts([], []).
checkAllRowCounts([FirstRow|OtherRows], [FirstCount|OtherCounts]) :-
    checkRowCount(FirstRow, FirstCount), checkAllRowCounts(OtherRows, OtherCounts).
    
checkColumnCount([], []).
checkColumnCount([RowHead|RowTail], [FirstCount|OtherCounts]) :-
    boat(RowHead), NextCount is FirstCount - 1, checkColumnCount(RowTail, OtherCounts).

checkAllColumnCounts([], Counts) :- allZero(Counts).
checkAllColumnCounts([FirstRow|OtherRows], Counts) :-
    checkColumnCount(FirstRow, Counts), checkColumnCount(OtherRows, Counts).
    

%CheckAround
%%(0,0)
checkAround(0, 0, Field) :- 
	getXYElement(0, 0, Field, E), 
	getXYElement(0, 1, Field, Down), 
	getXYElement(1, 0, Field, Right), 
	getXYElement(1, 1, Field, RightDown), 
	south(E, Down), 
	east(E, Right), 
	diagonal(E, RightDown).

%%(0, Y) met 0 < Y < width
checkAround(0, Y, Field) :- width(Field, Rows), Y > 0, Y < Rows, YS is Y-1, YA is Y+1, 
	getXYElement(0, Y, Field, E), 
	getXYElement(0, YS, Field, Up), 
	getXYElement(0, YA, Field, Down), 
	getXYElement(1, YS, Field, RightUp), 
	getXYElement(1, Y, Field, Right), 
	getXYElement(1, YA, Field, RightDown), 
	north(E, Up),south(E, Down),east(E, Right), diagonal(E, RightUp), diagonal(E, RightDown).

%%(X, 0) met 0 < X < height
checkAround(X, 0, Field) :- height(Field, Height), X > 0, X < Height, XS is X-1, XA is X+1, 
	getXYElement(X, 0, Field, E), 
	getXYElement(XS, 0, Field, Left), 
	getXYElement(XA, 0, Field, Right), 
	getXYElement(XS, 1, Field, LeftDown), 
	getXYElement(X, 1, Field, Down), 
	getXYElement(XA, 1, Field, RightDown), 
	west(E, Left), south(E, Down), east(E, Right), diagonal(E, LeftDown), diagonal(E, RightDown).

%%
checkAround(X, Y, Field) :- width(Field, Rows), height(Field, Columns), X > 0, Y > 0, X < Rows, Y < Columns, XS is X-1, YS is Y-1, XA is X+1, YA is Y+1, 
	getXYElement(X, Y, Field, E), 
	getXYElement(XS, YS, Field, LeftUp), 
	getXYElement(XS, Y, Field, Left), 
	getXYElement(XS, YA, Field, LeftDown), 
	getXYElement(X, YS, Field, Up), 
	getXYElement(X, YA, Field, Down), 
	getXYElement(XA, YS, Field, RightUp), 
	getXYElement(XA, Y, Field, Right), 
	getXYElement(XA, YA, Field, RightDown), 
	north(E, Up), east(E, Left), south(E, Down), west(E, Right), diagonal(E, LeftUp), diagonal(E, RightUp), diagonal(E, LeftDown), diagonal(E, RightDown).
