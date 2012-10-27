#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <stdbool.h>
#include <fcntl.h>

int main(int argc, char *argv[]){
  char *mem;
  struct stat sb;
  bool last = true;
  uint64_t i;
  int fd;

  fd  = open(argv[1], O_RDONLY);
  fstat(fd, &sb);
  mem = mmap(NULL, sb.st_size, PROT_WRITE, MAP_PRIVATE, fd, 0);

  if (mem == MAP_FAILED) {
    printf("error opening %s\n", argv[1]);
    return 1;
  }

  for(i=0;i<(uint64_t)sb.st_size;i++)
    if (last && mem[i] <= 122 && mem[i] >= 97)
      { mem[i]-=32; last=false; }
    else
      { last=(mem[i]==32); }

  write(1,mem,(uint64_t)sb.st_size);

  return 0;
}
