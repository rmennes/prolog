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


