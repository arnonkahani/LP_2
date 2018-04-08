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


nonogram_list([],C,C).

nonogram_list([1|T],C,Acc) :-
    Accn is Acc + 1,
    binary_list(T,C,Accn).

nonogram_list([0|T],C,Acc) :-
    binary_list(T,C,Acc).


nonogram_matrix(N,M,Matrix,OnesR,OnesC) :-
    length(Matrix,N),
    nonogram_matrix(M,Matrix,OnesR),
    transpose(Matrix,Ts),
    nonogram_matrix(N,Ts,OnesC).

nonogram_matrix(_,[],[]).

nonogram_matrix(N,[Mh|Mt],[Oh|Ot]) :-
    length(Mh,N),
    nonogram_list(Mh,Oh,0),
    nonogram_matrix(N,Mt,Ot).


sum_list([], 0).

sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

get_ones([H|T],I,C) :-
    is_list(H),
    sum_list(H,Sum),
    append(I,[Sum],Is),
    get_ones(T,Is,C).

get_ones([H|T],I,C) :-
    append(I,[H],Is),
    get_ones(T,Is,C). 

get_ones([],I,I).

% end utils

% Task 1



line_solution([Sh|St],C,D,Acc) :-
    Sh == 1,
    Cn is C + 1,
    line_solution(St,Cn,D,Acc).

line_solution([Sh|St],C,D,Acc) :-
    Sh == 0,
    C == 0,
    line_solution(St,0,D,Acc).

line_solution([Sh|St],C,D,Acc) :-
    Sh == 0,
    C \== 0,
    append(Acc,[C],Dn),
    line_solution(St,0,D,Dn).

line_solution([],0,D,D).

line_solution([],C,D,Acc) :-
    C \== 0,
    append(Acc,[C],Dn),
    line_solution([],0,D,Dn).

line_solution(S,D) :-
    line_solution(S,0,D,[]).

is_nonogram(0,[],[]).

is_nonogram(M,[Rh|Rt],[Sh|St]) :-
    line_solution(Sh,Rh),
    Ms is M - 1,
    is_nonogram(Ms,Rt,St).

is_nonogram(nonogram(N,M,RowData,ColData),Solution) :-
    get_ones(RowData,[],OnesR),
    get_ones(ColData,[],OnesC),
    nonogram_matrix(N,M,Solution,OnesR,OnesC),
    transpose(Solution,Ts),
    is_nonogram(N,RowData,Solution),
    is_nonogram(M,ColData,Ts).


nonogram_solve(nonogram(N,M,ColData,RowData), Solution) :-
    is_nonogram(nonogram(N,M,ColData,RowData),Solution).

