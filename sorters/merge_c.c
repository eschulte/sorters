#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
void merge (int *a, int *b, int n) {
  int i, *x, *p, *q;
  x = malloc(n * sizeof (int));
  for (i = 0, p = a, q = b; i < n; i++) 
    x[i] = p == b     ? *q++
      : q == a + n ? *p++
      : *p < *q    ? *p++
      :              *q++;
  memcpy(a, x, n * sizeof (int));
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
  int *lst;
  lst = malloc(sizeof(int)*argc-1);
  int i;
  for(i=1;i<argc;i++)
    lst[i-1] = atoi(argv[i]);
  merge_sort(lst, argc-1);
  for(i=1;i<argc;i++)
    printf("%d ", lst[i-1]);
  printf("\n");
  return 0;
}
