#define bufsize 2 << 15
int main(int argc, char *argv[]){
  int bread, fd; char buf[bufsize]; char *cur, *prev;
  fd = open(argv[1]);
  bread = read(fd, buf, bufsize);
  while(bread){
    cur = buf+1; prev = buf;
    while(cur < (buf + bread)){if(*prev==32 && *cur>=97 && *cur<=122) *cur -= 32; prev++; cur++;}
    write(1, buf, bread);
    bread = read(fd, buf, bufsize);
  }
}
