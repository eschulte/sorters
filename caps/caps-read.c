#define bufsize 2 << 15
int main(int argc, char *argv[]){
  int bread, fd; char buf[bufsize]; int i; char cur, prev;
  fd = open(argv[1]);
  bread = read(fd, buf, bufsize);
  while(bread){
    for(i=0; i<bread; i++){
      cur = buf[i];
      if(prev == 32 && cur >= 97 && cur <= 122) buf[i] -= 32;
      prev = cur;
    }
    write(1, buf, bread);
    bread = read(fd, buf, bufsize);
  }
}
