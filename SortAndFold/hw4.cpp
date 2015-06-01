/**
	Programming Languages
	hw4.h
	HW4: exercises using STL
		1. generate, shuffle, and sort contents of random access container
		2. generic foldl algorithm
	
	@author Geon Kang (NYUID: 17120399)
	@version 1.0 12/3/2013
*/
#include <iostream>
#include <algorithm>
#include <iterator>
#include <time.h>
#include <list>
#include "hw4.h"

using namespace std;

int add (int x, int y ) { return x+y; }

int main()
{

cout << "Output for Problem #1:" << endl;
int myArray[10]; // primitive array
const int N = 10;
// Note: Primitive arrays don’t have a begin() or end() function.
int* begin = myArray;
int* end = myArray + N;

//seed random number generator
srand(time(NULL));

//generate initial contents of random numbers
generate(begin,end,rand);

//randomly shuffle contents
random_shuffle(begin,end);

//sort contents in ascending order
sort(begin,end);

// Print the sorted sequence to standard out
copy(begin,end, ostream_iterator<int>(cout,"\n"));
cout << endl;

cout << "Output for Problem #2" << endl;
// array, Random Access container (a refinement of Forward Container)
int begins[] = { 1, 2, 3, 4, 5 };
int M = sizeof(begins) / sizeof(int);
// Should output 15
cout << foldl( begins, begins+M, add, 0 ) << endl;
// Linked list, a Sequence (refinement of Forward Container)
list<int> l;
l.push_back(1);
l.push_back(2);
l.push_back(3);
l.push_back(4);
l.push_back(5);
// Should output 15
cout << foldl( l.begin(), l.end(), add, 0 ) << endl;

return 0;
}