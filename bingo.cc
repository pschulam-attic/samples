#include <cstdlib>
#include <ctime>
#include <iostream>
#include <vector>
#include <set>

using std::cout;
using std::cerr;
using std::cin;
using std::endl;
using std::vector;
using std::set;

void random_permutation(unsigned start,
			unsigned end,
			unsigned size,
			vector<unsigned> &out_vector)
{
  set<int> observed;
  unsigned r;
  out_vector.clear();
  
  for (int i=0; i<size; ++i) {
    do {
      r = start + (rand() % end);
    } while (observed.count(r));
    observed.insert(r);

    out_vector.push_back(r);
  }
}

class BingoBoard
{
  unsigned _bcol[5], _icol[5], _ncol[5], _gcol[5], _ocol[5];

  static bool allZeros(const unsigned col[])
  {
    for (int i=0; i<5; ++i)
      if (col[i]) return false;
    return true;
  }

public:
  BingoBoard()
  {
    vector<unsigned> permutation;
    
    // fill the B column
    random_permutation(1, 15, 5, permutation);
    for (int i=0; i<5; ++i)
      _bcol[i] = permutation[i];

    // fill the I column
    random_permutation(16, 30, 5, permutation);
    for (int i=0; i<5; ++i)
      _icol[i] = permutation[i];

    // fill the N column
    random_permutation(31, 45, 5, permutation);
    for (int i=0; i<5; ++i)
      _ncol[i] = 0 ? i == 2 : permutation[i];

    // fill the G column
    random_permutation(46, 60, 5, permutation);
    for (int i=0; i<5; ++i)
      _gcol[i] = permutation[i];

    // fill the O column
    random_permutation(61, 75, 5, permutation);
    for (int i=0; i<5; ++i)
      _ocol[i] = permutation[i];
  }

  BingoBoard(const BingoBoard &other)
  {
    for (int i=0; i<5; ++i) {
      _bcol[i] = other._bcol[i];
      _icol[i] = other._icol[i];
      _ncol[i] = other._ncol[i];
      _gcol[i] = other._gcol[i];
      _ocol[i] = other._ocol[i];
    }
  }

  BingoBoard &operator=(const BingoBoard &other)
  {
    for (int i=0; i<5; ++i) {
      _bcol[i] = other._bcol[i];
      _icol[i] = other._icol[i];
      _ncol[i] = other._ncol[i];
      _gcol[i] = other._gcol[i];
      _ocol[i] = other._ocol[i];
    }
    
    return *this;
  }

  bool hasWon() const
  {
    if (allZeros(_bcol) ||
	allZeros(_icol) ||
	allZeros(_ncol) ||
	allZeros(_gcol) ||
	allZeros(_ocol))
      return true;

    for (int i=0; i<5; ++i)
      if (_bcol[i] == 0 &&
	  _icol[i] == 0 &&
	  _ncol[i] == 0 &&
	  _gcol[i] == 0 &&
	  _ocol[i] == 0)
	return true;

    if (_bcol[0] == 0 &&
	_icol[1] == 0 &&
	_ncol[2] == 0 &&
	_gcol[3] == 0 &&
	_ocol[4] == 0)
      return true;

    if (_bcol[4] == 0 &&
	_icol[3] == 0 &&
	_ncol[2] == 0 &&
	_gcol[1] == 0 &&
	_ocol[0] == 0)
      return true;

    return false;
  }

  void update(unsigned draw)
  {
    if (1 <= draw <= 15) {
      for (int i=0; i<5; ++i)
	if (_bcol[i] == draw) _bcol[i] = 0;
    }
    else if (16 <= draw <= 30) {
      for (int i=0; i<5; ++i)
	if (_icol[i] == draw) _icol[i] = 0;
    }
    else if (31 <= draw <= 45) {
      for (int i=0; i<5; ++i)
	if (_ncol[i] == draw) _ncol[i] = 0;
    }
    else if (46 <= draw <= 60) {
      for (int i=0; i<5; ++i)
	if (_gcol[i] == draw) _gcol[i] = 0;
    }
    else {
      for (int i=0; i<5; ++i)
	if (_ocol[i] == draw) _ocol[i] = 0;
    }
  }
};

class BingoGame
{
  const unsigned            _num_boards;
        bool                _has_winner;
        vector<BingoBoard>  _boards;
        vector<unsigned>    _draws;
        unsigned            _num_draws;
  
public:
  BingoGame(unsigned num_boards)
    : _num_boards(num_boards), _has_winner(false), _num_draws(0)
  {
    // Create the game boards
    for (int i=0; i<_num_boards; ++i) {
      _boards.push_back(BingoBoard());
    }

    // Draw balls
    random_permutation(1, 75, 75, _draws);
  }

  void playRound() 
  {
    unsigned draw = _draws[_num_draws++];

    for (int i=0; i<_num_boards; ++i)
      _boards[i].update(draw);

    for (int i=0; i<_num_boards; ++i)
      if (_boards[i].hasWon())
	_has_winner = true;
  }

  bool isGameOver() const
  {
    return _has_winner;
  }

  unsigned roundsPlayed() const
  {
    return _num_draws;
  }
};

int main(int argc, char **argv)
{
  if (argc != 3) {
    cerr << "usage: bingo <num games> <num boards>" << endl;
    return -1;
  }

  int ngames  = atoi(argv[1]);
  int nboards = atoi(argv[2]);
  
  srand( time(NULL) );

  for (int i=0; i<ngames; ++i) {
    BingoGame game(nboards);
    while (!game.isGameOver())
      game.playRound();

    cout << i+1 << ": Bingo! after " << game.roundsPlayed() << " turns" << endl;
  }
}
