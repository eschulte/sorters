#define bufsize 2 << 15
int main(){
  int bread; char buf[bufsize]; int i; char cur, prev;
  bread = read(0, buf, bufsize);
  while(bread){
    for(i=0; i<bread; i++){
      cur = buf[i];
      if(prev == 32 && cur >= 97 && cur <= 122) buf[i] -= 32;
      prev = cur;
    }
    write(1, buf, bread);
    bread = read(0, buf, bufsize);
  }
}
