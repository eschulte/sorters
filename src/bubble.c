#include <stdio.h>

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
  int lst[argc-1];
  int i;
  for(i=1;i<argc;i++)
    lst[i-1] = atoi(argv[i]);
  sort(lst, argc-1);
  for(i=1;i<argc;i++)
    printf("%d ", lst[i-1]);
  printf("\n");
}
