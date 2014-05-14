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

makeRowOfLength(0, _, []).
makeRowOfLength(N, P, [P|Tail]) :- N1 is N-1, makeRowOfLength(N1, P, Tail).

printRow([]) :- print('\n').
printRow([RowHead|RowTail]) :- print(RowHead), print(' '), printRow(RowTail).
printField([]).
printField([FirstRow|OtherRows]) :- printRow(FirstRow), printField(OtherRows).

omringRow(Row, P, [P|NewRow]) :- append(Row, [P], NewRow).

omringEveryRow([],_, []).
omringEveryRow([Row|Tail],P, [Result|ResultTail]) :- omringRow(Row, P, Result), omringEveryRow(Tail, P, ResultTail).
omringTable(Table, P, [R2|ResultTail]) :- 
	omringEveryRow(Table, P, [R1Head|R1Tail]), length(R1Head, L), makeRowOfLength(L, P, R2), append([R1Head|R1Tail], [R2], ResultTail),!.

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
    
%%%%%%%%%%%%%%%%%%%%%
% Check valid field %
%%%%%%%%%%%%%%%%%%%%%
checkRow([TopHead,'~'],[RowHead,'~'],[DownHead,'~']) :- north(RowHead,TopHead), south(RowHead,DownHead).
checkRow([TopPrevious,TopHead,TopNext|TopTail], [RowPrevious,RowHead,RowNext|RowTail], [DownPrevious,DownHead,DownNext|DownTail]) :-
	north(RowHead, TopHead), east(RowHead, RowNext), west(RowHead,RowPrevious),south(RowHead,DownHead),
	diagonal(RowHead,TopPrevious), diagonal(RowHead,TopNext), diagonal(RowHead,DownNext), diagonal(RowHead, DownPrevious), 
	checkRow([TopHead,TopNext|TopTail],[RowHead,RowNext|RowTail],[DownHead,DownNext|DownTail]).

checkColumn([TopHead,RowHead,LastHead]) :- 
	length(RowHead, Length), makeRowOfLength(Length, ~, LastHead), checkRow(TopHead, RowHead, LastHead). %MSS IS DEZE FOUT!
checkColumn([TopHead, RowHead, DownHead|Tail]) :-
	checkRow(TopHead, RowHead, DownHead), checkColumn([RowHead, DownHead|Tail]).	

checkField(Field) :- omringTable(Field, '~', Omring), checkColumn(Omring),!.


