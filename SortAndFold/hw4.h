/**
	Programming Languages
	hw4.h
	HW4: header file for generic fold algorithm
	
	@author Geon Kang (NYUID: 17120399)
	@version 1.0 12/3/2013
*/

template <typename Iter, typename BinF, typename Res>
Res foldl (Iter first, Iter last, BinF func, Res seed)
{
	//use seed to keep track of binary function's return value
	//keep passing seed with the value in the container to the binary function.
	//the value in the container is obtained by dereferencing the forward iterator first.
	//first is incremented so the next value in container can be accessed in the next "fold".
	//stop when first=last
	while(first != last) {
		seed = func(seed, *first);
		first++;
	}
	return seed;
}