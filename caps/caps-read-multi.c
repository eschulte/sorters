#define bufsize 2 << 24
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

void* do_cap(void* ptr);
char *buf;
int main(int argc, char *argv[]){
  unsigned int fd, chunksize, readsize, i, numthreads;
  numthreads = atoi(argv[2]); 
  pthread_t threads[numthreads];
  char* args[numthreads][2];
  buf = malloc(bufsize);
  fd = open(argv[1], O_RDONLY);
  readsize = read(fd, buf, bufsize);
  if(readsize == -1){printf("Error on read \n"); return -1;}
  chunksize = (readsize / numthreads);
  while(readsize){
    printf("Readsize: %u\n", readsize);
    for(i=0; i<numthreads; i++){
      args[i][0] = buf + (i * chunksize);
      if(i == (numthreads - 1)) args[i][1] = buf + readsize;
      else args[i][1] = buf + (i+1) * chunksize; 
      printf("Chunksize: %d\n", chunksize);
      printf("Buf: %p. Thread %d gets %p - %p \n", buf, i, args[i][0], args[i][1]);
      pthread_create(threads + i, NULL, do_cap, (void*)args[i]);
    }
    
    for(i=0; i<numthreads; i++) pthread_join(threads[i], NULL);    
    
    write(1, buf, readsize);
    readsize = read(fd, buf, bufsize);
  }
  return 0;
}

void *do_cap(void *ptr){
  char **args = (char**)ptr;
  char *cur = args[0] + 1;
  char *prev = args[0];
  char *last = args[1];  
  while(cur < last){
    if(*prev == ' ' && *cur <= 'z' && *cur >= 'a') *cur -= 32;
    prev ++; cur ++;
  }
  return NULL;
}



