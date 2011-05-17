#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
int *locmerge(int *l, unsigned int ll,
	      int *r, unsigned int lr)
{
  int *result;
  unsigned int il, ir, ires;
 
  il = ir = ires = 0;
 
  result = malloc(sizeof(int)*(ll+lr));
  while ( (ll > 0) && ( lr > 0 ) ) {
    if ( l[il] <= r[ir] ) {
      result[ires++] = l[il++]; ll--;
    } else {
      result[ires++] = r[ir++]; lr--;
    }
  }
  if ( ll > 0 ) {
    memcpy(&result[ires], &l[il], sizeof(int)*ll);
  }
  if ( lr > 0 ) {
    memcpy(&result[ires], &r[ir], sizeof(int)*lr);
  }
  return result;
}
 
 
int *mergesort(int *list, unsigned int l)
{
  unsigned int middle;
  int *tleft, *tright, *result, *left, *right;
 
  if ( l < 2 ) return list;
  middle = l / 2;
  tleft = malloc(sizeof(int)*(l - middle));
  tright = malloc(sizeof(int)*middle);
  memcpy(tleft, list, sizeof(int)*(l-middle));
  memcpy(tright, list+(l-middle), sizeof(int)*middle);
  left = mergesort(tleft, l-middle);
  right = mergesort(tright, middle);
  if (left[l-middle-1] <= right[0])
    result = list;
  else
    result = locmerge(left, l-middle, right, middle);
  if ( tleft == left ) {
    free(left);
  } else {
    free(left); free(tleft);
  }
  if ( tright == right ) {
    free(right);
  } else {
    free(right); free(tright);
  }
  return result;
}

int main(int argc, char *argv[]) {
  int *lst;
  lst = malloc(sizeof(int)*argc-1);
  int i;
  for(i=1;i<argc;i++)
    lst[i-1] = atoi(argv[i]);
  lst = mergesort(lst, argc-1);
  for(i=1;i<argc;i++)
    printf("%d ", lst[i-1]);
  printf("\n");
  return 0;
}
