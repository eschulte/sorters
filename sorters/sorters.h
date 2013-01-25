#define NUM 100

int file_to_int_list(char* file, int* lst){
  FILE *f;
  int i, nextint;

  f = fopen(file, "r");
  for(i=1;i<NUM && !feof(f);i++)
    if (fscanf(f, "%d", &nextint) != EOF)
      lst[i-1] = nextint;

  return (i-2);
}
