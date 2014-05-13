%Input grammar
boat(n).
boat(e).
boat(s).
boat(w).
boat(x).
water('~').

%%north
north(n, '~').
north(o, '~').
north(e, '~').
north(s, n).
north(s, x).
north(w, '~').
north(x, _).
north('~', _).

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

%ElementsOnBorders
%%LeftBorder
leftBorder(n).
leftBorder(s).
leftBorder(e).
leftBorder(o).
leftBorder(x).
leftBorder('~').

%%RightBorder
rightBorder(n).
rightBorder(s).
rightBorder(w).
rightBorder(o).
rightBorder(x).
rightBorder('~').

%%TopBorder
topBorder(n).
topBorder(e).
topBorder(w).
topBorder(o).
topBorder(x).
topBorder('~').

%%ButtomBorder
buttomBorder(s).
buttomBorder(e).
buttomBorder(w).
buttomBorder(o).
buttomBorder(x).
buttomBorder('~').

%%LeftTopCorner
leftTopCorner(n).
leftTopCorner(e).
leftTopCorner(o).
leftTopCorner('~').

%%RightTopCorner
rightTopCorner(n).
rightTopCorner(w).
rightTopCorner(o).
rightTopCorner('~').

%%LeftButtomCorner
leftButtomCorner(s).
leftButtomCorner(e).
leftButtomCorner(o).
leftButtomCorner('~').

%%RightButtonCorner
rightButtomCorner(s).
rightButtomCorner(w).
rightButtomCorner(o).
rightButtomCorner('~').


