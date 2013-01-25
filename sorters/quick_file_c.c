#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sorters.h"
 
void quick(int *left, int *right)
{
  if (right > left) {
    int pivot = left[(right-left)/2];
    int *r = right, *l = left;
    do {
      while (*l < pivot) l++;
      while (*r > pivot) r--;
      if (l <= r) {
        int t = *l;
        *l++ = *r;
        *r-- = t;
      }
    } while (l <= r);
    quick(left, r);
    quick(l, right);
  }
}

void sort(int *array, int length)
{
  quick(array, array+length-1);
}

int main(int argc, char *argv[]) {
  int lst[NUM];
  int i, length;

  length = file_to_int_list(argv[1], lst);

  sort(lst, length);
  for(i=0;i<length;i++) printf("%d\n", lst[i]);

  return 0;
}
