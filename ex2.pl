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
        transpose_helper(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

transpose_helper([], [], []).

transpose_helper([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        transpose_helper(Rest, Fs, Oss).


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
    construct_matrix(M,N,ColData,Ts).
	
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


insert_hint(R, [0|Solution]):-
	insert_hint(R, Solution).
	
fill_with_k([], _, 0).
fill_with_k([H|T], H, N) :- 
    Nn is N - 1,
    fill_with_k(T, H, Nn).


 %Task 3


 reverse_1([],Z,Z).

 reverse_1([H|T],Z,Acc) :- reverse_1(T,Z,[H|Acc]).




verify_ramsey(r(S,T,N), Solution, ramsey):-
    get_word_list(N,S,L1),
    get_word_list(N,T,L2),
    hasnt_clique(L1,Solution,0),
    hasnt_clique(L2,Solution,1).

verify_ramsey(r(S,T,N), Solution, Counter):-
    verify_ramsey_h(r(S,T,N), Solution, Counter).


verify_ramsey_h(r(S,_,N), Solution, CounterExample):-
    get_word_list(N,S,L),
    has_clique(L,Solution,0),
    check_solution(L, Solution, CounterExample,0).


verify_ramsey_h(r(_,T,N), Solution, Ce):-
    get_word_list(N,T,L),
    has_clique(L,Solution,1),
    check_solution(L, Solution, Ce,1).

hasnt_clique([V|Rest],Sol,Num):-
    \+is_clique(V,Sol,Num),
    hasnt_clique(Rest,Sol,Num).

hasnt_clique([],_,_).


has_clique([V|_],Sol,Num):-
    is_clique(V,Sol,Num).

has_clique([_|Rest],Sol,Num):-
    has_clique(Rest,Sol,Num).

has_clique([],_,_):-
    false.


check_solution([V|_], Sol, Vs,Num):-
    is_clique(V,Sol,Num),
    reverse_1(V,Vs,[]).


check_solution([_|Rest], Sol, Vs,Num):-
    check_solution(Rest, Sol, Vs,Num).



is_clique([],_,_).

is_clique([V|Rest],Sol,Num):-
    has_edge(V,Rest,Sol,Num),
    is_clique(Rest,Sol,Num).

has_edge(V,Vs,Sol,Num):-
    Vs2 is V-1,
    get_vertex_list(Vs2,Sol,Vlist),
    check_edges(Vlist,Vs,Num).

get_vertex_list(0,[H|_],H).

get_vertex_list(V,[_|Rest],Vlist):-
    V > 0,
    Vs is V-1,
    get_vertex_list(Vs,Rest,Vlist).

check_edges(_,[],_).

check_edges(Vlist,[H|Rest],Num):-
    Hs is H-1,
    check_for_edge(Vlist,Hs,Num),
    check_edges(Vlist,Rest,Num).

check_for_edge([_|Rest],V,Num):-
    V>0,
    Vs is V-1,
    check_for_edge(Rest,Vs,Num).

check_for_edge([Num|_],0,Num).


first(K,[K|Rest]):-
    Ks is K-1,
    Ks > 0 ,
    first(Ks, Rest).

first(1,[1]).

last_e(N,1,[N]).

last_e(N,K,[N|Rest]):-
    Ns is N-1,
    Ks is K-1,
    Ks >0,
    last_e(Ns,Ks,Rest).


incr(_,[],[]).

incr(N,[K|Rest],[Ks|Rest]):-
    Ks is K+1,
    K<N,
    N > 0.

incr(N,[K1,K2|R],[K1s, K2s|Rs]):-
    Ns is N-1,
    K1 == N,
    incr(Ns,[K2|R],[K2s|Rs]),
    K1s is K2s+1,
    N>0.


get_word_list(N,K,[First|Rest]):-
    first(K,First),
    last_e(N,K,Last),
    get_words(N,First,Last,Rest).

get_words(_,L,L,[]).

get_words(N,Previous, Last, [Next|Rest]):-
    incr(N,Previous, Next),
    get_words(N,Next, Last, Rest).




%Task 4

find_ramsey(r(S,T,N),Sol):-
    get_all_ramsey(N,Rlist),
    find_solution(r(S,T,N),Rlist,Sol).



find_solution(r(S,T,N),[Sol|_],Sol):-
    verify_ramsey(r(S,T,N),Sol,ramsey).


find_solution(r(S,T,N),[_|Rest],Sol):-
    find_solution(r(S,T,N),Rest,Sol).
find_solution(_,[],_):- false.

get_all_ramsey(N,Rlist):-
    get_word_list(N,2,Edges),
    subset_3(Edges,Elist),
    elist_to_mlist(N,Elist,Rlist).

elist_to_mlist(_,[],[]).

elist_to_mlist(N,[H1|R1],[H2|R2]):-
    edges_to_matrix(N,H1,H2),
    elist_to_mlist(N,R1,R2).

edges_to_matrix(N,L1,L2):-
    zero_matrix(N,N,M),
    edges_matrix(M,L1,L2).

edges_to_matrix(_,[],[]).

zero_matrix(0,_,[]).

zero_matrix(M,N,[H|Rest]):-
    n_zeros(N,H),
    M>0,
    Ms is M-1,
    zero_matrix(Ms,N,Rest).

n_zeros(0,[]).

n_zeros(N,[0|R]):-
    N>0,
    Ns is N-1,
    n_zeros(Ns,R).

edges_matrix(M,[],M).

edges_matrix(M,[Edge|Rest],Me):-
    add_edge(M,Edge,Re),
    edges_matrix(Re,Rest,Me).

add_edge(M,[I,J],Me):-
    Is is I-1,
    Js is J-1,
    add_it(Is,J,M,Re),
    add_it(Js,I,Re,Me).

add_it(0,J,[H|R],[Hs|R]):-
    Js is J-1,
    add_j_to_list(Js,H,Hs).

add_it(I,J,[H|R1],[H|R2]):-
    I>0,
    Is is I-1,
    add_it(Is,J,R1,R2).

add_j_to_list(0,[_|R],[1|R]).

add_j_to_list(J,[H|R1],[H|R2]):-
    J>0,
    Js is J-1,
    add_j_to_list(Js,R1,R2).


subset_2([], []).

subset_2([E|Tail], [E|NTail]):-
  subset_2(Tail, NTail).

subset_2([_|Tail], NTail):-
  subset_2(Tail, NTail).

subset_3(L,A) :-
   bagof(B,subset_2(L,B),A).


subset_2([], [],_).

subset_2([E|Tail], [-E|NTail],Num):-
  Num == -1,
  subset_2(Tail, NTail,Num).

subset_2([E|Tail], [E|NTail],Num):-
  Num == 1,
  subset_2(Tail, NTail,Num).


subset_2([_|Tail], NTail,Num):-
  subset_2(Tail, NTail,Num).

subset_3(L,A,Num) :-
   bagof(B,subset_2(L,B,Num),A).


%Part 3

%Task 5

encode_ramsey(r(S,T,N),Map,CNF):-
    encode_map(N,Map),
    map_to_cnf(Map,S,T,CNF).


encode_map(N,Map):-
    Ns is N * (N-1),
    Nss is Ns / 2,
    create_map(Nss, Map).

create_map(0,[]).

create_map(N,[_|L]):-
    N>0,
    Ns is N-1,
    create_map(Ns,L).


map_to_cnf(Map,S,T,CNF):-
    subset_3(Map,L1),
    get_n_size(L1,S,L1s),
    get_n_size(L1,T,L2s,-1),
    append(L1s,L2s,CNF).


get_n_size([],_,[]).

get_n_size([H|R1],N,[H|R2]):-
    length(H,N),
    get_n_size(R1,N,R2).

get_n_size([H|R1],N, L2):-
    length(H,Ns),
    N \==Ns,
    get_n_size(R1,N,L2).

get_n_size([],_,[],_).

get_n_size([H|R1],N,[Hs|R2],Num):-
    length(H,N),
    change_co(H,Num,Hs),
    get_n_size(R1,N,R2,Num).

get_n_size([H|R1],N, L2,Num):-
    length(H,Ns),
    N \==Ns,
    get_n_size(R1,N,L2,Num).


change_co([],_,[]).

change_co([H|R1],Num,[-H|R2]):-
    change_co(R1,Num,R2).

%Task 6

decode_ramsey(Map,Solution):-
    length(Map,N),
    get_graph_size(N,En),
    get_word_list(En,2,Edges),
    get_graph_edges(Edges,Map,Ge),
    edges_to_matrix(En,Ge,Solution).

get_graph_size(N,En):-
    S1 is 8*N,
    S2 is S1+1,
    S3 is sqrt(S2),
    S4 is S3 +1,
    En is round(S4/2).


get_graph_edges([],[],[]).

get_graph_edges([Edge|Re],[H|Rm],[Edge|Ra]):-
    H ==1,
    get_graph_edges(Re,Rm,Ra).

get_graph_edges([_|Re],[H|Rm],Sol):-
    H == -1,
    get_graph_edges(Re,Rm,Sol).


%Task 7

solve_ramsey(r(S,T,N), Solution):-
    encode_ramsey(r(S,T,N),Map,CNF),
    sat(CNF),
    decode_ramsey(Map,Solution).

first_set([H|R1],K,[H|R2]):-
    Ks is K-1,
    K > 0 ,
    first_set(R1,Ks, R2).

first_set(_,0,[]).

last_set(L,K,Sol):-
    reverse_1(L,Ls,[]),
    first_set(Ls,K,Lss),
    reverse_1(Lss,Sol,[]).


incr_set(_,[],[]).

get_ks([K,Ks|_],K,Ks).

get_ks([H|Rest],K,Ks):-
    H\==K,
    get_ks(Rest,K,Ks).

incr_set(Set,[K|Rest],[Ks|Rest]):-
    get_ks(Set,K,Ks)),
    N \== [].

incr_set(N,[K1,K2|R],[K1s, K2s|Rs]):-
    Ns is N-1,
    K1 == N,
    incr_set(Ns,[K2|R],[K2s|Rs]),
    K1s is K2s+1,
    N>0.


get_word_list_set(Set,K,[First|Rest]):-
    first_set(Set,K,First),
    last_set(Set,K,Last),
    get_words_set(Set,First,Last,Rest).

get_words_set(_,L,L,[]).

get_words_set(N,Previous, Last, [Next|Rest]):-
    incr_set(N,Previous, Next),
    get_words_set(N,Next, Last, Rest).