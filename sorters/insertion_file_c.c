#include <stdio.h>
#include "sorters.h"

void sort(int a[], int size)
{
  int i, j, temp;
   
  for(i=1; i<=size-1; i++)
    {
      temp = a[i];
      j = i-1;
      while(j>=0 && a[j] > temp)
        {
          a[j+1] = a[j];
          j -= 1;
        }
      a[j+1] = temp;
    }
}

int main(int argc, char *argv[]) {
  int lst[NUM];
  int i, length;
  length = file_to_int_list(argv[1], lst);

  sort(lst, length);
  for(i=0;i<length;i++) printf("%d\n", lst[i]);

  return 0;
}
