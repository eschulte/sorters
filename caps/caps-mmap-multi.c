#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <stdbool.h>
#include <fcntl.h>
#include <pthread.h>

char *mem;
void *do_cap(void *args);

int main(int argc, char *argv[]){
  struct stat sb;
  pthread_t t0,t1,t2,t3;
  int fd;
  uint64_t size,chunk;
  uint64_t a0[2],a1[2],a2[2],a3[2];

  fd  = open(argv[1], O_RDONLY);
  fstat(fd, &sb);
  mem = mmap(NULL, sb.st_size, PROT_WRITE, MAP_PRIVATE, fd, 0);

  if (mem == MAP_FAILED) {
    fprintf(stderr, "error opening %s\n", argv[1]);
    return 1;
  }

  size  = (uint64_t)sb.st_size;
  chunk = (size / 4);
  /* fprintf(stderr, "size:%lu chunk:%lu\n", size, chunk); */

  /* Launch Threads */
  a0[0]=(0*chunk); a0[1]=(1*chunk)-1;
  a1[0]=(1*chunk); a1[1]=(2*chunk)-1;
  a2[0]=(2*chunk); a2[1]=(3*chunk)-1;
  a3[0]=(3*chunk); a3[1]=sb.st_size;

  pthread_create(&t0, NULL, do_cap, (void*)a0);
  pthread_create(&t1, NULL, do_cap, (void*)a1);
  pthread_create(&t2, NULL, do_cap, (void*)a2);
  pthread_create(&t3, NULL, do_cap, (void*)a3);

  /* Join Threads */
  pthread_join(t0, NULL);
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);
  pthread_join(t3, NULL);

  /* Write Output */
  write(1,mem,size);

  return 0;
}

void *do_cap(void *ptr){
  bool last = true;
  uint64_t i;
  uint64_t *args;
  args = (uint64_t *)ptr;
  /* fprintf(stderr,"%lu -> %lu\n", args[0], args[1]); */
  for(i=args[0];i<args[1];i++)
    if (last && mem[i] <= 122 && mem[i] >= 97)
      { mem[i]-=32; last=false; }
    else
      { last=(mem[i]==32); }
  return NULL;
}
