// Square_board.h
// Michael E. Sparks, 11-16-16

// Accommodates square boards having side lengths from 1 up to 0xFFFF units.
// Whether the host machine can acommodate boards of that size is another
// matter.
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

    bool* board {nullptr}; // num(positions) = side_length ** 2

    void clear_board(void); // set all positions to false

    void print_board(void);
};

// The following macro allows us to index a one-dimensional
// dynamically allocated array as if it were a two-dimensional,
// fixed-size array. With C's malloc, it's possible to create
// a dynamic array of pointers, each of which can be used to
// allocate a dynamic array of unit type elements. That's great
// for jagged-edge arrays, for example. (Here, we wish to focus
// on C++'s new/delete, though.) This indexing approach is also
// more performant, as data are stored contiguously in memory.
// Extension of this concept for indexing one-dimensional
// memory as yet higher-order arrays (e.g., 3D, 4D) is left
// as an exercise.
#define IND(R, C, LEN) R * LEN + C
