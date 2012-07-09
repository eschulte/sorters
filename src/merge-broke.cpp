#include <iostream>
#include <cmath>
#include <stdlib.h>
using namespace std;

void merge(int* input, int p, int r)
{
  int mid = floor((p + r) / 2);
  int i1 = 0;
  int i2 = p;
  int i3 = mid + 1;

  // Temp array
  int temp[r-p+1];

  // Merge in sorted form the 2 arrays
  while ( i2 <= mid && i3 <= r )
    if ( input[i2] < input[i3] )
      temp[i1++] = input[i2++];
    else
      temp[i1++] = input[i3++];

  // Merge the remaining elements in left array
  while ( i2 <= mid )
    temp[i1++] = input[i2++];

  // Merge the remaining elements in right array
  while ( i3 <= r )
    temp[i1++] = input[i3++];

  // Move from temp array to master array
  for ( int i = p; i <= r; i++ )
    input[i] = temp[i-p];
}

void merge_sort(int* input, int p, int r)
{
  if ( p < r )
    {
      int mid = floor((p + r) / 2);
      merge_sort(input, p, mid);
      merge_sort(input, mid + 1, r);
      merge(input, p, r);
    }
}

int main(int argc, char *argv[])
{
  int *lst;
  int i;
  lst = (int*)malloc(sizeof(int)*argc-1);
  for(i=1;i<argc;i++) lst[i-1] = atoi(argv[i]);
  merge_sort(lst, 0, (argc-1));
  for(i=0;i<(argc-1);i++) cout << lst[i] << " ";
  cout << endl;
  return 0;
}
