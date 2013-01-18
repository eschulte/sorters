#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM 100

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
  int i, j, nextint;
  FILE *f;

  if (argc < 2){
    printf("Specify the file of integers as the first argument.\n");
    return 1;
  }

  f = fopen(argv[1], "r");
  for(i=1;i<NUM && !feof(f);i++)
    if (fscanf(f, "%d", &nextint) != EOF)
      lst[i-1] = nextint;

  merge_sort(lst, i-2);
  for(j=1;j<i-1;j++)
    printf("%d ", lst[j-1]);
  printf("\n");

  fclose(f);
  return 0;
}
