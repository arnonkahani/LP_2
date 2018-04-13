% Tests
nonogramPuzzle(easy,1,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows=4,
    Cols=4,
    ColsBlocks=[[3],[1,1],[1,1],[2]],
    RowsBlocks=[[4],[1,1],[2],[1]].

nonogramPuzzle(easy,2,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows=5,
    Cols=5,
    ColsBlocks=[[3],[4],[5],[4],[3]],
    RowsBlocks=[[1],[3],[5],[5],[5]].

nonogramPuzzle(easy,4,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):- % 2 solutions
    Rows=8,
    Cols=7,
    ColsBlocks=[[2],[1,1],[2],[2,4],[1,1,2],[1,1,1,1],[2,2]],
    RowsBlocks=[[2],[1,1],[1,1],[2],[2,1],[1,2,2],[4,1],[3]].

nonogramPuzzle(medium,3,nonogram(Rows,Cols,ColsBlocks,RowsBlocks)):-
    Rows = 10,
    Cols = 10,
    ColsBlocks = [[1, 3, 3], [2, 3, 1], [1, 1, 1], [6], [5, 3], [2, 1, 3], [4, 3], [5, 3], [4, 4], [3, 4]],
    RowsBlocks = [[3, 6], [1, 6], [1, 1, 4], [5, 3], [2, 2, 1], [3, 2, 2], [2, 4], [1, 7], [1, 3, 3], [2, 1]].
% utils

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


% end utils

% Task 1

line_solution([],[],0).

line_solution([Dh|Dt],[],C) :-
    Dh == C,
    line_solution(Dt,[],0).

line_solution(D,[1|St],C) :-
    Cn is C + 1,
    line_solution(D,St,Cn).

line_solution([Dh|Dt],[0|St],C) :-
    C \== 0,
    C == Dh,
    line_solution(Dt,St,0).

line_solution(D,[0|St],C) :-
    C == 0,
    line_solution(D,St,0).


is_nonogram(0,_,[],[]).

is_nonogram(NumOfRows,NumOfCols,[Rh|Rt],[Sh|St]) :-
    length(Sh,NumOfCols),
    line_solution(Rh,Sh,0),
    Ns is NumOfRows - 1,
    is_nonogram(Ns,NumOfCols,Rt,St).

is_nonogram(nonogram(N,M,ColData,RowData),Solution) :-
    length(Solution,N),
    transpose(Solution,Ts),
    is_nonogram(N,M,RowData,Solution),
    is_nonogram(M,N,ColData,Ts).

% Task 2
nonogram_solve(nonogram(N,M,ColData,RowData), Solution):-
	construct_matrix(N,M,RowData,Solution),
    transpose(Solution,Ts),
    is_nonogram(M,N,ColData,Ts).
	
construct_matrix(N,M,[Rh|Rt],[Sh|St]):-
	N > 0,
    length(Sh, M), 
    insert_hint(Rh, Sh),
	Nn is N - 1,
	construct_matrix(Nn,M,Rt,St).

construct_matrix(0,_,[],[]).

	
insert_hint([Rh1,Rh2|Rt], Solution):-
	length(H, Rh1), 
	fill_with_k(H,1,Rh1),
	append(H, [0|N], Solution), 
	insert_hint([Rh2|Rt], N).


insert_hint([R], Solution):-
	length(H, R), 
	fill_with_k(H, 1, R),
	append(H, N, Solution),
    length(N,Nl), 
	fill_with_k(N, 0, Nl).


insert_hint(RowData, [0|Solution]):-
	insert_hint(RowData, Solution).
	
fill_with_k([], _, 0).
fill_with_k([H|T], H, N) :- 
    Nn is N - 1,
    fill_with_k(T, H, Nn).