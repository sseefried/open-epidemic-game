#include <stdio.h>

double float2double(float f) {
  return (double)f;
}

void set_no_buffering() {
  setvbuf(stdout, NULL, _IONBF, 0);
}
