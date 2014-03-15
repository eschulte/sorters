#include <stdio.h>
#include "../sorters/sorters.h"

int main(int argc, char *argv[]){
  int a[NUM], b[NUM];
  int total_bad, length_a, length_b, tmp_a, i, j;
  /* parse input files into lists */
  length_a = file_to_int_list(argv[1], a);
  length_b = file_to_int_list(argv[2], b);
  /* loop through each collecting bad comparisons */
  total_bad=0;
  for(i=0; i<length_a; i++){
    tmp_a=a[i];
    for(j=0; j<length_a; j++){
      /* truncated second list */
      if(j > length_b){
        total_bad++; }
      /* both lists are large enough */
      else {
        if(j < i){              /* bigger element too early in list */
          if(b[j] > tmp_a){
            total_bad++; } }
        else if(j > i){
          if(b[j] < tmp_a){     /* smaller element too late in list */
            total_bad++; } }
        else {
          if(b[j] != tmp_a){    /* different element in same place */
            total_bad++; } } } } }
  return total_bad; }
