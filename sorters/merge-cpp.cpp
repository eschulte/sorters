#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm> // for std::inplace_merge
#include <functional> // for std::less

template<typename RandomAccessIterator, typename Order>
 void mergesort(RandomAccessIterator first,
                RandomAccessIterator last,
                Order order)
{
  if (last - first > 1)
  {
    RandomAccessIterator middle = first + (last - first) / 2;
    mergesort(first, middle, order);
    mergesort(middle, last, order);
    std::inplace_merge(first, middle, last, order);
  }
}

template<typename RandomAccessIterator>
 void mergesort(RandomAccessIterator first,
                RandomAccessIterator last)
{
  mergesort(first, last, std::less<typename std::iterator_traits<RandomAccessIterator>::value_type>());
}

template< typename TYPE >
void
print( TYPE val )
{
  std::cout << val << " " ;
}

int
main(int argc, char *argv[])
{
  std::vector<int> lst(argc-1);
  int i;
  for(i=1;i<argc;i++) lst[i-1] = atoi(argv[i]);
  mergesort(lst.begin(), lst.end());
  std::for_each( &lst[0], &lst[argc-1], print<int>);
  std::cout << std::endl;
}
