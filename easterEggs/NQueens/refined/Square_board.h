// Square_board.h
// Michael E. Sparks, 11-19-16

// Accommodates square boards having side lengths from 1 up to 0xFFFF units.
// Whether the host machine can actually acommodate boards withing that
// size range is of course another matter.
typedef unsigned short N_t;

// The Square_board class implements a square board suitable
// for representing a chess or checkers game state, for example.
// It is intended for use as a base class only.
class Square_board
{
  public:

    N_t get_side_length(void); // allow users to learn side_length

  protected:

    static const N_t default_size {8}; // 8-Queens is the typical form

    N_t side_length {default_size};

    bool *board {nullptr}; // num(positions) = side_length ** 2

    void clear_board(void); // set all positions to false

    void mirror_board_Yaxis(void); // Flip on east-west axis

    void mirror_board_Xaxis(void); // Flip on north-south axis

    void print_board(void);

  private:

    N_t& N=side_length; // a reference to save on typing!
};

// The following macro allows us to index a one-dimensional
// dynamically allocated array as if it were a two-dimensional,
// fixed-size array. With C's malloc, it's possible to create
// a dynamic array of pointers, each of which can be used to
// allocate a dynamic array of unit type elements. That's great
// for jagged-edge arrays, for example. (Here, we wish to use
// C++'s new & delete, though.) This indexing approach is also
// usually more performant, as data are stored contiguously in
// memory.
#define IND(R, C, LEN) (R) * (LEN) + (C)
