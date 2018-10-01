% N, a nonnegative integer specifying the size of the square grid.
% T, a list of N lists, each representing a row of the square grid. Each row is represented by a list of N distinct integers from 1 through N. The corresponding columns also contain all the integers from 1 through N.
% C, a structure with function symbol counts and arity 4. Its arguments are all lists of N integers, and represent the tower counts for the top, bottom, left, and right edges, respectively.
tower(N, T, C) :-
  % C = counts([],[],[],[]).
  length(T, N),
  C = counts(TOP,BOTTOM,LEFT,RIGHT),
  length(LEFT, N),
  length(RIGHT, N),
  length(TOP, N),
  length(BOTTOM, N),
  maplist(getSize(N), T),
  maplist(domain(N), T),
  maplist(fd_all_different, T),
  transpose(T, T2),
  maplist(fd_all_different, T2),
  constraints(T2, TOP, N), %process cols
  constraints_reverse(T2, BOTTOM, N), %reverse cols
  constraints(T, LEFT, N), %process rows
  constraints_reverse(T, RIGHT, N), %reverse rows
  maplist(fd_labeling, T).

getSize(Size, Object) :-
    length(Object, Size).

constraints([], _, N).
constraints(T, CList, N) :-
  T = [Head| Tail],
  %CList is TOP, BOTTOM....
  CList = [Fst| Rest], %Fst = Count
  % get_count(Head, Fst), %Head is each row or columns
  get_count(Head, Count, 0),
  Fst is Count,
  constraints(Tail, Rest, N).


constraints_reverse([], _, N).
constraints_reverse(T, CList, N) :-
    T = [Head| Tail],
    reverse(Head, X),
    %CList is TOP, BOTTOM....
    CList = [Fst| Rest], %Fst = Count
    % get_count(Head, Fst), %Head is each row or columns
    get_count(X, Count, 0),
    Fst = Count,
    constraints_reverse(Tail, Rest, N).

get_count([], 0, _).
get_count([Fst|Rest], Count, Tallest) :-
    Fst #< Tallest,
    get_count(Rest, Count, Tallest);
    Fst #> Tallest,
    NewCount #= Count - 1,
    get_count(Rest, NewCount, Fst).


plain_tower(N, T, C) :-
      length(T, N),
      maplist(getSize(N), T),
      % all_different(L),
      build_list(L, N),
      % all_different(L),
      maplist(permutation(L), T),
      all_different(T),
      transpose(T,T2),
      % all_different(T2),
      maplist(permutation(L), T2),
      all_different(T2),

      C = counts(TOP,BOTTOM,LEFT,RIGHT),
      length(LEFT, N),
      length(RIGHT, N),
      length(TOP, N),
      length(BOTTOM, N),
      constraints(T, LEFT, N), %process rows
       %reverse
      constraints(T2, TOP, N), %process cols
      constraints_reverse(T, RIGHT, N),
      constraints_reverse(T2, BOTTOM, N).

build_list(L, N) :-
  Goal = between(1, N, X),
  findall(X, Goal, L).

all_different([]).
all_different([Head|Tail]) :-
    \+(member(Head, Tail))
    , !, all_different(Tail).


ambiguous(N, C, T1, T2) :-
  tower(N, T1, C),
  tower(N, T2, C),
  T1 \= T2.

domain(N, []).
domain(N, [H|T]) :-
  fd_domain(H, 1, N),
  domain(N, T).

speedup(Z):-
  get_time(X, tower(4, T1,C1)),
  get_time(Y, plain_tower(4,T2,C2)),
  Z is X/Y.

get_time(X, Fn) :-
  statistics(cpu_time, [_|X]), Fn, !.


% transpose copied pasted from SWI prolog
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
