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
    subset_3(Map,L1,1),
    get_n_size(L1,S,L1s),
    subset_3(Map,L2,-1),
    get_n_size(L2,T,L2s),
    append(L1s,L2s,CNF).

get_n_size([],_,[]).

get_n_size([H|R1],N,[H|R2]):-
    length(H,N),
    get_n_size(R1,N,R2).

get_n_size([H|R1],N, L2):-
    length(H,Ns),
    N \==Ns,
    get_n_size(R1,N,L2).



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
















