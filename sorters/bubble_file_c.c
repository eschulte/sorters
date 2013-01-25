#include <stdio.h>
#include "sorters.h"

void swap(int *p)
{
  int t = p[0];
  p[0] = p[1];
  p[1] = t;
}
   
void sort(int *a, int size)
{
  int i,sorted;
  do {
    sorted = 1;
    --size;
    for (i=0; i<size; i++)
      if (a[i+1] < a[i])
        {
          swap(a+i);
          sorted = 0;
        }
  } while (!sorted);
}

int main(int argc, char *argv[]) {
  int lst[NUM];
  int i, length;
  length = file_to_int_list(argv[1], lst);

  sort(lst, length);
  for(i=0;i<length;i++) printf("%d\n", lst[i]);
}
