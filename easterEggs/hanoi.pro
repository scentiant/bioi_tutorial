% Michael E. Sparks

% Predicate to denote leaf nodes of solution tree
hanoi(1,A,B,_) :-
  !,
  write(A),
  write(' -> '),
  write(B),
  nl.

% Predicate implementing internal nodes of solution
% tree--these are expanded into leaf nodes and/or
% additional internal nodes
hanoi(N,L,R,C) :-
  N > 1,
  N1 is N - 1,
  hanoi(N1,L,C,R),
  hanoi(1,L,R,_),
  hanoi(N1,C,R,L).
