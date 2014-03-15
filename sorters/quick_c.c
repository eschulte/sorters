#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
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
  int lst[argc-1];
  int i;
  for(i=1;i<argc;i++)
    lst[i-1] = atoi(argv[i]);
  sort(lst, argc-1);
  for(i=1;i<argc;i++)
    printf("%d ", lst[i-1]);
  printf("\n");
  return 0;
}
