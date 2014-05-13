%Utils
getNthElement(0, [Head|_], Head).
getNthElement(N, [_|Tail], X) :- N1 is N-1, getNthElement(N1, Tail, X).

getXYElement(X, 0, [Head|_], E) :- getNthElement(X, Head, E).
getXYElement(X, Y, [_|Tail], E) :- Y1 is Y-1, getXYElement(X, Y1, Tail, E).

%Default rules
%%Row and Coumns
rows([Head|_], X) :- length(Head, X). %highest row number
columns(Field, X) :- length(Field, X). %highest column number

%%Nord
nord(n, '~').
nord(o, '~').
nord(e, '~').
nord(s, n).
nord(s, x).
nord(w, '~').
nord(x, _).
nord('~', _).

%%East
east(n, '~').
east(o, '~').
east(e, '~').
east(s, '~').
east(w, x).
east(w, e).
east(x, _).
east('~', _).

%%South
south(n, x).
south(n, s).
south(o, '~').
south(e, '~').
south(s, '~').
south(w, '~').
south(x, _).
south('~', _).

%%West
west(n, '~').
west(o, '~').
west(e, x).
west(e, w).
west(s, '~').
west(w, '~').
west(x, _).
west('~', _).

%%diagonal
diagonal(_, '~').
diagonal('~', _).

checkAround(X, Y, Field) :- rows(Field, Rows), columns(Field, Columns), X > 0, Y > 0, X < Rows, Y < Columns, XS is X-1, YS is Y-1, XA is X+1, YA is Y+1, getXYElement(X, Y, Field, E), getXYElement(XS, YS, Field, LeftUp), getXYElement(XS, Y, Field, Left), getXYElement(XS, YA, Field, LeftDown), getXYElement(X, YS, Field, Up), getXYElement(X, YA, Field, Down), getXYElement(XA, YS, Field, RightUp), getXYElement(XA, Y, Field, Right), getXYElement(XA, YA, Field, RightDown), nord(E, Up), east(E, Left), south(E, Down), west(E, Right), diagonal(E, LeftUp), diagonal(E, RightUp), diagonal(E, LeftDown), diagonal(E, RightDown).
