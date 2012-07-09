// adapted from http://www.cplusplus.happycodings.com/Algorithms/code17.html
#include <iostream>
#include <stdlib.h>
int a[50];

void merge(int,int,int);

void merge_sort(int low,int high){
  int mid;
  if(low<high)
    {
      mid=(low+high)/2;
      merge_sort(low,mid);
      merge_sort(mid+1,high);
      merge(low,mid,high);
    }
}

void merge(int low,int mid,int high){
  int h,i,j,b[50],k;
  h=low;
  i=low;
  j=mid+1;

  while((h<=mid)&&(j<=high))
    {
      if(a[h]<=a[j])
        {
          b[i]=a[h];
          h++;
        }
      else
        {
          b[i]=a[j];
          j++;
        }
      i++;
    }
  if(h>mid)
    {
      for(k=j;k<=high;k++)
        {
          b[i]=a[k];
          i++;
        }
    }
  else
    {
      for(k=h;k<=mid;k++)
        {
          b[i]=a[k];
          i++;
        }
    }
  for(k=low;k<=high;k++) a[k]=b[k];
}

int main(int argc, char *argv[])
{
  int i;
  for(i=1;i<argc;i++) a[i-1] = atoi(argv[i]);
  merge_sort(0, (argc - 1));
  for(i=1;i<argc;i++) std::cout << a[i-1] << " ";
  std::cout << std::endl;
  return 0;
}
