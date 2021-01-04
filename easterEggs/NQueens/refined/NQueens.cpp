// NQueens.cpp
// Michael E. Sparks, 11-19-16

#include <iostream>
#include "NQueens.h"

using namespace std;

// Because this predicate can score both complete and partial
// solutions, it enables a backtracking-type approach to probing
// the solution space, in contrast to enumerating and testing
// every potential puzzle solution (i.e., brute force methods).
bool NQueens::is_it_feasible(const bool* soln, const N_t& N)
{
  bool q_seen{false};

  // Test for multiple queens on a given row
  for(auto i{0};i<N;++i,q_seen=false)
    for(auto j{0};j<N;++j)
      if(soln[IND(i,j,N)]) {
        if(q_seen)
          return false;
        else
          q_seen=true;
      }

  // Test for multiple queens on a given column
  for(auto j{0};j<N;++j,q_seen=false)
    for(auto i{0};i<N;++i)
      if(soln[IND(i,j,N)]) {
        if(q_seen)
          return false;
        else
          q_seen=true;
      }

  // Test for multiple queens on diagonals
  // parallel to the main diagonal.
  // Cases where diagonals include first column...
  for(auto i{N-2};i>=0;--i,q_seen=false)
    for(auto ii{i},j{0};ii<N;++ii,++j)
      if(soln[IND(ii,j,N)]) {
        if(q_seen)
          return false;
        else
          q_seen=true;
      }
  // ...and the rest.
  for(auto j{1};j<N-1;++j,q_seen=false)
    for(auto jj{j},i{0};jj<N;++jj,++i)
      if(soln[IND(i,jj,N)]) {
        if(q_seen)
          return false;
        else
          q_seen=true;
      }

  // Test for multiple queens on diagonals
  // parallel to the antidiagonal.
  // Cases where antidiagonals include last column...
  for(auto i{N-2};i>=0;--i,q_seen=false)
    for(auto ii{i},j{N-1};ii<N;++ii,--j)
      if(soln[IND(ii,j,N)]) {
        if(q_seen)
          return false;
        else
          q_seen=true;
      }
  // ...and the rest.
  for(auto j{N-2};j>0;--j,q_seen=false)
    for(auto jj{j},i{0};jj>=0;--jj,++i)
      if(soln[IND(i,jj,N)]) {
        if(q_seen)
          return false;
        else
          q_seen=true;
      }

  return true;
}

// Verifies that a feasible solution has all queens placed.
// This could be used as a sanity check above and beyond using
// the is_it_feasible predicate on the last row. Doing so is
// really not necessary when using the recurse_over_solns member,
// though, as you would have already placed N-1 queens on the board
// in a satisfactory configuration before ever getting a chance to
// position the Nth queen.
bool NQueens::is_it_complete(const bool* soln, const N_t& N)
{
  N_t count{0};

  for(auto i{0};i<N;++i)
    for(auto j{0};j<N;++j)
      if(soln[IND(i,j,N)])
        ++count;

  return (count==N && is_it_feasible(soln,N));
}

// Recursion allows generalization of N-Queens to any "reasonable" N.
void NQueens::recurse_over_solns(const N_t& row,const N_t& N)
{
  for(auto i{0};i<N;board[IND(row,i++,N)]=false) {
    board[IND(row,i,N)]=true;

    // Premature optimization is the root of all evil, but though
    // here we're not interested in writing code that runs like
    // cheap paint, there's also no sensible reason to reject the
    // speedup afforded by (partially) exploiting the N-Queens
    // domain's peculiar properties regarding mirror images of
    // solutions. Yet further optimization is possible through
    // use of memoization and/or coroutines, for example.
    if(is_it_feasible(board,N)) {
      if(row==N-1) {
        print_board();

        if(N>1 && !(N%2 && board[IND(0,N/2,N)])) {
          mirror_board_Yaxis();
          print_board();
          mirror_board_Yaxis(); // undo in-place alterations
        }
      }
      else if (row>0 || i < N/2 + N%2)
        recurse_over_solns(row+1,N);
    }
  }

  return;
}

// interactively set N.
N_t NQueens::set_N(void)
{
  side_length=0;
  while(!side_length) {
    cout << "Please enter N (.GT. 0) for NQueens: ";
    cin >> side_length; // yes, this is dangerous
  }

  return (N_t)side_length;
}

// passively/ programmatically set N.
N_t NQueens::set_N(N_t proposed_N)
{
  try {
    // yes, this is also dangerous
    if((side_length=proposed_N)<=0)
      throw domain_error("N .LE. 0");
  }
  catch(domain_error& e) {
    cerr << "*** Warning: " << e.what()
         << " ; using " << default_size
         << " instead of " << side_length
         << " ***" << endl;
    side_length=default_size;
  }

  return (N_t)side_length;
}

// Manages the process of enumerating N-Queens solutions.
// Note that we're leaving plenty of optimization tricks on
// the table that would be evident upon studying the N-Queens
// problem in greater detail.
// What we're after here is more to demonstrate the generality
// achievable with using a recursive solution, to get a sense
// of backtracking search through assistance of a predicate
// filter, etc.
void NQueens::enumerate_NQueens_solutions(void)
{
  N_t& N=side_length; // a local reference to save on typing!
  try {
    // For N-Queens, we're dealing with sparse matrices, and
    // there are more space-efficient representations available
    // than that used by the Square_board base class. It's
    // perhaps more intuitive to think in terms of a full
    // N-by-N grid, though.
    board=new bool[N*N];
  }
  catch(bad_alloc&) {
    cerr << "Insufficient memory to allocate "
         << N << " X " << N << " board\n";
    return;
  }

  clear_board();

/*
  // Here's a known solution for N .EQ. 8 :
  if(N==default_size) {
    board[IND(0,5,N)]=true;
    board[IND(1,0,N)]=true;
    board[IND(2,4,N)]=true;
    board[IND(3,1,N)]=true;
    board[IND(4,7,N)]=true;
    board[IND(5,2,N)]=true;
    board[IND(6,6,N)]=true;
    board[IND(7,3,N)]=true;

    print_board();
  }
*/

/*
  // Here's an approach to find all such solutions,
  // specifically all 92 for the 8-Queens problem:
  if(N==default_size) {

    for(auto i{0};i<N;
        board[IND(0,i++,N)]=false) { // first row
      board[IND(0,i,N)]=true;
      if(!is_it_feasible(board,N)) continue; // unnecessary here

      for(auto j{0};j<N;
          board[IND(1,j++,N)]=false) { // second
        board[IND(1,j,N)]=true;
        if(!is_it_feasible(board,N)) continue;

        for(auto k{0};k<N;
            board[IND(2,k++,N)]=false) { // third
          board[IND(2,k,N)]=true;
          if(!is_it_feasible(board,N)) continue;

          for(auto l{0};l<N;
              board[IND(3,l++,N)]=false) { // fourth
            board[IND(3,l,N)]=true;
            if(!is_it_feasible(board,N)) continue;

            for(auto m{0};m<N;
                board[IND(4,m++,N)]=false) { // fifth
              board[IND(4,m,N)]=true;
              if(!is_it_feasible(board,N)) continue;

              for(auto n{0};n<N;
                  board[IND(5,n++,N)]=false) { // sixth
                board[IND(5,n,N)]=true;
                if(!is_it_feasible(board,N)) continue;

                for(auto m{0};m<N;
                    board[IND(6,m++,N)]=false) { // seventh
                  board[IND(6,m,N)]=true;
                  if(!is_it_feasible(board,N)) continue;

                  for(auto n{0};n<N;
                      board[IND(7,n++,N)]=false) { // eighth (& last)
                    board[IND(7,n,N)]=true;
                    if(!is_it_complete(board,N)) continue;

                    print_board();
    }}}}}}}}
  }
*/

  // Now we need to generalize the pattern shown above
  // so that it can be applied to arbitrary values of N.
  // Given the "similarity" of the nested for-loops,
  // a recursive solution to the problem should be evident.
  recurse_over_solns(0,N);

  delete [] board;

  return;
}
