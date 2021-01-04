/*
  Michael E. Sparks, 11-30-20

  Demo of SWI Prolog's Constraint Logic Programming library
  (for real-valued data).

SAMPLE USAGE, DEMONSTRATING THAT CLP ALLOWS TO SOLVE FOR
VARIABLES BOTH "FORWARDS" AND "BACKWARDS":

?- speed_of_light(C), E0 = 0.25 * C * C, E is E0.
C = 299792458,
E0 = 0.25*299792458*299792458,
E = 2.246887946842044e+16.

?- relativity1(E,0.25).
E = 2.246887946842044e+16.

?- relativity2(E,0.25).
E = 2.246887946842044e+16 ;
false.

?- relativity1(2.25e+16,M).
ERROR: Arguments are not sufficiently instantiated
ERROR: In:
ERROR:   [11] 2.25e+16 is _6628*299792458*299792458
ERROR:    [9] <user>
ERROR:
ERROR: Note: some frames are missing due to last-call optimization.
ERROR: Re-run your program in debug mode (:- debug.) to get more detail.

?- relativity2(2.25e+16,M).
M = 0.25034626261206416 ;
false.

*/

% see https://www.swi-prolog.org/pldoc/man?section=clp
:- use_module(library(clpr)).

speed_of_light(299792458). % meters per second

% Energy expressed in joules, Mass in kilograms
relativity1(Energy,Mass) :-
    speed_of_light(C),
    Energy is Mass * C * C.

% as above, but using a CLP constraint rather than
% a Prolog arithmetic formula to express the
% relationship between Energy & Mass.
relativity2(Energy,Mass) :-
    speed_of_light(C),
    {Energy = Mass * C * C}.
