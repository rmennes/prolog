%Utils
getNthElement(0, [Head|_], Head).
getNthElement(N, [_|Tail], X) :- N1 is N-1, getNthElement(N1, Tail, X).

getXYElement(X, 0, [Head|_], E) :- getNthElement(X, Head, E).
getXYElement(X, Y, [_|Tail], E) :- Y1 is Y-1, getXYElement(X, Y1, Tail, E).

%Default rules
%%Row and Coumns
width([Head|_], X) :- length(Head, X1), X is X1-1. %width
height(Field, X) :- length(Field, X1), X is X1-1. %height

%Check whether field satisfies the numbers on the side
checkFieldStrip([], 0).
checkFieldStrip([StripHead|StripTail], BoatCount) :- 
    boat(StripHead), NextCount is BoatCount - 1, checkFieldStrip(StripTail, NextCount);
    water(StripHead), checkFieldStrip(StripTail, BoatCount).
   
%CheckAround
%%(0,0)
checkAround(0, 0, Field) :- 
	getXYElement(0, 0, Field, E), 
	getXYElement(0, 1, Field, Down), 
	getXYElement(1, 0, Field, Right), 
	getXYElement(1, 1, Field, RightDown), 
	leftTopCorner(E),
	leftBorder(Down),
	topBorder(Right).
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
	leftBorder(E), leftBorder(Up), leftBorder(Down),
	north(E, Up),south(E, Down),east(E, Right), diagonal(E, RightUp), diagonal(E, RightDown).

%%(X, 0) met 0 < X < height
checkAround(X, 0, Field) :- height(Field, Height), X > 0, X < Height, XS is X-1, XA is X+1, 
	getXYElement(X, 0, Field, E), 
	getXYElement(XS, 0, Field, Left), 
	getXYElement(XA, 0, Field, Right), 
	getXYElement(XS, 1, Field, LeftDown), 
	getXYElement(X, 1, Field, Down), 
	getXYElement(XA, 1, Field, RightDown), 
	topBorder(E), topBorder(Left), topBorder(Right),
	west(E, Left), south(E, Down), east(E, Right), diagonal(E, LeftDown), diagonal(E, RightDown).

%%(X, Y) met 0 < X < height && 0 < Y < width
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

%%(X, Y) met X = height && 0 < Y < width
checkAround(X, Y, Field) :- width(Field, Width), height(Field, X), Y > 0, Y < Width, XS is X-1, YS is Y-1, YA is Y+1,
	getXYElement(X, Y, Field, E),
	getXYElement(X, YS, Field, Up),
	getXYElement(X, YA, Field, Down),
	getXYElement(XS, YS, Field, LeftUp),
	getXYElement(XS, Y, Field, Left),
	getXYElement(XS, YA, Field, LeftDown),
	rightBorder(E), rightBorder(Up), rightBorder(Left),
	north(E, Up), south(E, Down), west(E, Left), diagonal(E, LeftUp), diagonal(E, LeftDown).

%%(X, Y) met 0 < X < height && Y = width
checkAround(X, Y, Field) :- width(Field, Y), height(Field, Height), X > 0, X < Height, XS is X-1, XA is X+1, YS is Y-1,
	getXYElement(X, Y, Field, E),
	getXYElement(XS, YS, Field, LeftUp),
	getXYElement(XS, Y, Field, Left),
	getXYElement(X, YS, Field, Up),
	getXYElement(XA, Y, Field, Right),
	getXYElement(XA, YS, Field, RightUp),
	buttomBorder(E), buttomBorder(Left), buttom(Right),
	north(E, Up), west(E, Left), east(E, Right), diagonal(E, LeftUp), diagonal(E, RightUp).

%%(X, Y) met X - heigth && Y = width
checkAround(X, Y, Field) :- width(Field, Y),height(Field, X), XS is X-1, YS is Y-1,
	getXYElement(X, Y, Field, E),
	getXYElement(XS, YS, Field, LeftUp),
	getXYElement(XS, Y, Field, Left),
	getXYElement(X, YS, Field, Up),
	buttomRightCorner(E), buttomBorder(Left), rightBorder(Up),
	north(E, Up), west(E, Left), diagonal(E, LeftUp).
