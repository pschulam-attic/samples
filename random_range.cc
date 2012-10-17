#include <cstdlib>
#include <ctime>
#include <iostream>

using std::cout;
using std::endl;

unsigned rand_range(unsigned start, unsigned end)
{
  if (end <= start) return 0;

  unsigned span = end - start;
  return start + (rand() % span);
}

int main()
{
  srand( time(NULL) );

  for (int i=0; i<10; ++i)
    cout << rand_range(10, 20) << endl;
}
