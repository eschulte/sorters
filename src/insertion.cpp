#include <iostream>
#include <algorithm>
#include <vector>

template<typename Iter>
void insertion_sort(Iter beg, Iter end)
{
    for (Iter i = beg; i != end; ++i)
        std::rotate(std::lower_bound(beg, i, *i), i, i+1);
}

template< typename TYPE >
void
print( TYPE val )
{
  std::cout << val << " ";
}

int
main(int argc, char *argv[])
{
  std::vector<int> lst(argc-1);
  int i;
  for(i=1;i<argc;i++) lst[i-1] = atoi(argv[i]);
  insertion_sort(lst.begin(), lst.end());
  std::for_each( &lst[0], &lst[argc-1], print<int>);
  std::cout << std::endl;
}
