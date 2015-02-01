
#include <Foundation/Foundation.h>

const char *resource_path() {
  NSString *s = [[NSBundle mainBundle] resourcePath];
  return [s UTF8String];
}