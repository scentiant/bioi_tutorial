// Square_board.cpp
// Michael E. Sparks, 11-16-16

#include <iostream>
#include "Square_board.h"

using namespace std;

// Allow users of the class to learn side_length
N_t Square_board::get_side_length(void)
{
  return side_length;
}

// Initialize the board, setting all positions to false
void Square_board::clear_board(void)
{
  for(auto i{0};i<side_length;++i)
    for(auto j{0};j<side_length;++j)
      board[IND(i,j,side_length)]=false;

  return;
}

// Report configuration to stdout
void Square_board::print_board(void)
{
  cout << endl;
  for(auto i{0};i<side_length;++i)
    for(auto j{0};j<side_length;++j) {
      cout << (board[IND(i,j,side_length)] ? 'Q' : '-') << " ";
      if(j==side_length-1)
        cout << endl;
    }
  cout << endl;

  return;
}
