#include <iostream>
#include <algorithm>
 
template< typename ARRAY_TYPE, typename INDEX_TYPE >
void
bubble_sort( ARRAY_TYPE array[], INDEX_TYPE size )
{
  bool done = false ;
 
  while( !done )
    {
      done = true ;
      for( INDEX_TYPE i = 0 ; i < size-1 ; i++ )
        {
          if( array[i] > array[i+1] )
            {
              done = false ;
              std::swap(array[i], array[i+1]);
            }
        }
    }
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
  int lst[argc-1];
  int i;
  for(i=1;i<argc;i++)
    lst[i-1] = atoi(argv[i]);
  bubble_sort( lst, argc-1 ) ;
  std::for_each( &lst[0], &lst[argc-1], print<int> ) ;
  std::cout << std::endl ;
}
