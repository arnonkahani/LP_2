% Tests


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

is_binary(0).
is_binary(1).

is_binary_list([H|T]) :-
    is_binary(H),
    is_binary_list(T).

is_binary_list([]).

size_of_matrix(N,M,Matrix) :-
    length(Matrix,N),
    size_of_matrix(M,Matrix).

size_of_matrix(N,[Mh|Mt]) :-
    length(Mh,N),
    is_binary_list(Mh),
    size_of_matrix(N,Mt).

size_of_matrix(_,[]).

is_equal([A|As],[B|Bs]) :-
    A == B,
    is_equal(As,Bs).

is_equal([],[]).

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



is_nonogram(nonogram(N,M,ColData,RowData),Solution) :-
    length(ColData,M),
    length(RowData,N),
    size_of_matrix(N,M,Solution),
    is_nonogram(N,RowData,Solution),
    transpose(Solution,Ts),
    is_nonogram(M,ColData,Ts).

nonogram_solve(nonogram(N,M,ColData,RowData), Solution) :-
    is_nonogram(nonogram(N,M,ColData,RowData),Solution).

