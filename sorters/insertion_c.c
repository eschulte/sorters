#include <stdio.h>
   
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
