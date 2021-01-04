// Square_board.cpp
// Michael E. Sparks, 11-19-16

#include <iostream>
#include "Square_board.h"

using namespace std;

// Allow users of the class to learn side_length
N_t Square_board::get_side_length(void)
{
  return N; // equivalent to "return side_length;"
}

// Initialize the board, setting all positions to false
void Square_board::clear_board(void)
{
  for(auto i{0};i<N;++i)
    for(auto j{0};j<N;++j)
      board[IND(i,j,N)]=false;

  return;
}

// In-place flip of board in an east-west manner
void Square_board::mirror_board_Yaxis(void)
{
  for(auto i{0};i<N;++i)
    for(auto j{0};j<N/2;++j) {
      auto tmp=board[IND(i,j,N)];
      board[IND(i,j,N)]=board[IND(i,N-1-j,N)];
      board[IND(i,N-1-j,N)]=tmp;
    }

  return;
}

// In-place flip of board in a north-south manner
void Square_board::mirror_board_Xaxis(void)
{
  for(auto j{0};j<N;++j)
    for(auto i{0};i<N/2;++i) {
      auto tmp=board[IND(i,j,N)];
      board[IND(i,j,N)]=board[IND(N-1-i,j,N)];
      board[IND(N-1-i,j,N)]=tmp;
    }

  return;
}

// Report configuration to stdout
void Square_board::print_board(void)
{
  cout << endl;
  for(auto i{0};i<N;++i)
    for(auto j{0};j<N;++j) {
      cout << (board[IND(i,j,N)] ? 'Q' : '-') << " ";
      if(j==N-1)
        cout << endl;
    }
  cout << endl;

  return;
}
