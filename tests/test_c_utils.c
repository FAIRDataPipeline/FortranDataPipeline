#include "test_c_utils.h"

#include <string.h>

int is_foo(const char* str){
  return strcmp(str, "foo") == 0;
}

void repeat_str(const char* input, char* output){
  strcpy(output, input);
  strcat(output, input);
}
