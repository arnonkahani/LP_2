:- use_module(naive_sat).
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

%%%%%%%%%%%%%%%%%%%%%%%%%Part 2%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

%Task 3


 reverse_1([],Z,Z).

 reverse_1([H|T],Z,Acc) :- reverse_1(T,Z,[H|Acc]).

verify_ramsey(r(S,_,N), Solution, CounterExample):-
    get_word_list(N,S,L),
    check_solution(L, Solution, CounterExample,0).


verify_ramsey(r(_,T,N), Solution, Ce):-
    get_word_list(N,T,L),
    check_solution(L, Solution, Ce,1).

check_solution([V|_], Sol, Vs,Num):-
    is_clique(V,Sol,Num),
    reverse_1(V,Vs,[]).

check_solution([H|Rest],Sol,V,Num):-
    H \== V,
    check_solution(Rest,Sol,V,Num).

check_solution([],_,ramsey,_).

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
    get_all_ramsey(Rlist,N),
    check_all(r(S,T,N),Rlist,Sol).

check_all(r(S,T,N),[Sol|_],Sol):-
    verify_ramsey(r(S,T,N),Sol,ramsey).

check_all(r(S,T,N),[H|Rest],Sol):-
    H \== Sol,
    check_all(r(S,T,N),Rest,Sol).


get_all_ramsey(Rlist,N):-
    get_word_list(N,2,Edges),
    all_subsets(Edges,Elist),
    elist_to_mlist(N,Elist,Rlist).

elist_to_mlist(N,[H1],[H2]):-
    edges_to_matrix(N,H1,H2).

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
    Ms is M-1,
    zero_matrix(Ms,N,Rest).

n_zeros(0,[]).

n_zeros(N,[0|R]):-
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

all_subsets(_,[]).


all_subsets(Edges,[H|L]):-
    powerset(Edges,H),
    \+member(H,L),
    all_subsets(Edges,L).




powerset(L, [H|T]):-
  append([H|T], _, L).
powerset([_|L], P):-
  powerset(L, P).


first_set(N,[K|R1],[K|R2]):-
    Ns is N-1,
    Ns > 0 ,
    first_set(Ns, R1,R2).

first_set(1,[K|_],[K]).


last_set(K,In,Out):-
   reverse_1(In,Ls,[]),
   first_set(K,Ls,Rl),
   reverse_1(Rl , Out,[]).


incr_set(_,[],[]).

incr_set(N,[K|Rest],[Ks|Rest]):-
    Ks is K+1,
    K<N,
    N > 0.

incr_set(N,[K1,K2|R],[K1s, K2s|Rs]):-
    Ns is N-1,
    K1 == N,
    incr_set(Ns,[K2|R],[K2s|Rs]),
    K1s is K2s+1,
    N>0.


get_words_set(N,K,[First|Rest]):-
    first(K,First),
    last_e(N,K,Last),
    get_words(N,First,Last,Rest).

get_words_set(_,L,L,[]).

get_words_set(N,Previous, Last, [Next|Rest]):-
    incr(N,Previous, Next),
    get_words(N,Next, Last, Rest).




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
    get_words_set(Map,S,L1),
    get_words_set(Map,T,L2),
    append(L1,L2,CNF).
















