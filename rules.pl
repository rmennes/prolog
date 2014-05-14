%%%%%%%%%%%
% UTILITY %
%%%%%%%%%%%

getNthElement(0, [Head|_], Head).
getNthElement(N, [_|Tail], X) :- N1 is N-1, getNthElement(N1, Tail, X).

getXYElement(X, 0, [Head|_], E) :- getNthElement(X, Head, E).
getXYElement(X, Y, [_|Tail], E) :- Y1 is Y-1, getXYElement(X, Y1, Tail, E).

%Default rules
%%Row and Coumns
width([Head|_], X) :- length(Head, X1), X is X1-1. %width
height(Field, X) :- length(Field, X1), X is X1-1. %height

actualWidth([Head|_], X) :- length(Head, X).
actualHeight(Field, X) :- length(Field, X).

allZero([]).
allZero([0|T]) :- allZero(T).

makeRowOfLength(0, _, []).
makeRowOfLength(N, P, [P|Tail]) :- N1 is N-1, makeRowOfLength(N1, P, Tail).

omringRow(Row, P, [P|NewRow]) :- append(Row, [P], NewRow).

omringEveryRow([],_, []).
omringEveryRow([Row|Tail],P, [Result|ResultTail]) :- omringRow(Row, P, Result), omringEveryRow(Tail, P, ResultTail).
omringTable(Table, P, [R2|ResultTail]) :- 
	omringEveryRow(Table, P, [R1Head|R1Tail]), length(R1Head, L), makeRowOfLength(L, P, R2), append([R1Head|R1Tail], [R2], ResultTail),!.

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
    
loopY(_, 0, _, 0).  % Loop until x=0 && count=0
loopY(Field, Count, X, Y) :- Count > 0, Y > 0,
    XDec is X - 1, YDec is Y - 1, CountDec is Count - 1, getXYElement(XDec, YDec, Field, Cell), boat(Cell), loopY(Field, CountDec, X, YDec);
    XDec is X - 1, YDec is Y - 1, getXYElement(XDec, YDec, Field, Cell), water(Cell), loopY(Field, Count, X, YDec).
   
  
loopX(Field, _, XInc) :- X is XInc - 1, actualWidth(Field, X).
loopX(Field, [FirstCount|OtherCounts], X) :-
    actualHeight(Field, Height), loopY(Field, FirstCount, X, Height), XInc is X + 1, loopX(Field, OtherCounts, XInc).
    
checkAllColumnCounts(Field, ColumnCounts) :-
    loopX(Field, ColumnCounts, 1).
    
checkAllCounts(Field, RowCounts, ColumnCounts, Field) :- checkAllRowCounts(Field, RowCounts), checkAllColumnCounts(Field, ColumnCounts).

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


