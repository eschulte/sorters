#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sorters.h"

void merge (int *a, int *b, int n) {
  int i, *x, *p, *q;
  x = malloc(n * sizeof (int));
  for (i = 0, p = a, q = b; i < n; i++) 
    x[i] = p == b     ? *q++
      : q == a + n ? *p++
      : *p < *q    ? *p++
      :              *q++;
  //memcpy(a, x, n * sizeof (int));
  for (i = 0; i < n; i++) {
    a[i] = x[i];
  }
  free(x);
}   
 
void merge_sort (int *a, int n) {
  int *b, m;
  if (n < 2)
    return;
  m = n / 2;
  b = a + m;
  merge_sort(a, m);
  merge_sort(b, n - m);
  merge(a, b, n);
}

int main(int argc, char *argv[]) {
  int lst[NUM];
  int i, length;
  length = file_to_int_list(argv[1], lst);

  merge_sort(lst, length);
  for(i=0;i<length;i++) printf("%d\n", lst[i]);

  return 0;
}
