#include <stdio.h>

#define SPACE    32
#define LOWER_A  97
#define LOWER_Z  122
#define DOWNCASE 32

int main(int argc, char *argv[])
{
  FILE *file;
  int index, character, previous;
  previous = SPACE;
  file = fopen(argv[1], "r");
  while((character = getc(file)) != EOF){
    if(previous == SPACE && LOWER_A <= character && character <= LOWER_Z){
      putchar(character - DOWNCASE);
    } else {
      putchar(character);
    }
    previous = character;
  }
  return 0;
}
