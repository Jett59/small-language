#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

void panic(const char *msg) {
  printf("PANIC: %s\n", msg);
  abort();
}

int getInt(void) {
  int i;
  scanf("%d", &i);
  return i;
}
