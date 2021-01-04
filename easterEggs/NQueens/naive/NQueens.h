// NQueens.h
// Michael E. Sparks, 11-16-16

#include "Square_board.h"

// Objects of type NQueens exhaustively enumerate
// all solutions to the N-Queens puzzle. 
class NQueens : public Square_board
{
  private:

    // Predicate tests whether a solution (partial
    // up through complete) is potentially correct.
    bool is_it_feasible(const bool* soln, const N_t& N);

    // Test if a solution to the puzzle both has N queens
    // placed and is feasible.
    bool is_it_complete(const bool* soln, const N_t& N);

    // Auxiliary function used by enumerate_NQueens_solutions
    void recurse_over_solns(const N_t& row,const N_t& N);

  public:

    // set_N uses run-time polymorphism / overloading to
    // give user code the option of setting N (to something
    // other than default_size) interactively or passively.
    // Neither of these member functions is written in an
    // especially robust manner, as this app is just a toy.
    N_t set_N(void); // interactive
    N_t set_N(N_t proposed_N); // passive/ programmatic

    // This method builds a set of all possible solutions
    // satisfying the underlying constraints of this puzzle.
    void enumerate_NQueens_solutions(void);
};
