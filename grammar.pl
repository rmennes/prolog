%Input grammar
boat(n).
boat(e).
boat(s).
boat(w).
boat(x).
boat(o).
water('~').

%%north
north(n, '~').
north(o, '~').
north(e, '~').
north(s, n).
north(s, x).
north(w, '~').
north(x, x).
north(x, '~').
north(x, n).
north('~', '~').
north('~', s).
north('~', e).
north('~', w).
north('~', o).
north('~', x).

%%East
east(n, '~').
east(o, '~').
east(e, '~').
east(s, '~').
east(w, x).
east(w, e).
east(x, x).
east(x, '~').
east(x, e).
east('~', '~').
east('~', s).
east('~', n).
east('~', w).
east('~', o).
east('~', x).

%%South
south(n, x).
south(n, s).
south(o, '~').
south(e, '~').
south(s, '~').
south(w, '~').
south(x, x).
south(x, '~').
south(x, s).
south('~', '~').
south('~', e).
south('~', n).
south('~', w).
south('~', o).
south('~', x).


%%West
west(n, '~').
west(o, '~').
west(e, x).
west(e, w).
west(s, '~').
west(w, '~').
west(x, x).
west(x, '~').
west(x, w).
west('~', '~').
west('~', e).
west('~', n).
west('~', s).
west('~', o).
west('~', x).

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
leftTopCorner(w).
leftTopCorner(o).
leftTopCorner('~').

%%RightTopCorner
rightTopCorner(n).
rightTopCorner(e).
rightTopCorner(o).
rightTopCorner('~').

%%LeftButtomCorner
leftButtomCorner(s).
leftButtomCorner(w).
leftButtomCorner(o).
leftButtomCorner('~').

%%RightButtonCorner
rightButtomCorner(s).
rightButtomCorner(e).
rightButtomCorner(o).
rightButtomCorner('~').


