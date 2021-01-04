// driver_app.cpp
// Michael E. Sparks, 11-19-16

// Program to enumerate all solutions to the N-Queens
// problem, for any sensible values of N.

#include <iostream>
#include <cstdlib>
#include "NQueens.h"

// Driver application
int main(int argc,char **argv)
{
  NQueens my_nqueens;

  // Two calls to atoi seem wasteful, but the
  // N_t type used by NQueens objects is of
  // an unsigned type. If the shell passes in
  // a negative number, bad things will happen.
  if(argc > 1 && atoi(argv[1]) > 0)
    my_nqueens.set_N((N_t)atoi(argv[1]));
  // else we just use the NQueens class'
  // default value of N

  std::cout << std::endl << "Solutions to "
            << my_nqueens.get_side_length()
            << "-Queens:" << std::endl;

  my_nqueens.enumerate_NQueens_solutions();

  exit(EXIT_SUCCESS);
}
