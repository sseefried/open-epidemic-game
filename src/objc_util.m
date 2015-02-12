#include <Foundation/Foundation.h>

const char *ios_resource_path() {
  NSString *s = [[NSBundle mainBundle] resourcePath];
  return [s UTF8String];
}

void ns_log(const char *s) {
  NSString *str = [NSString stringWithUTF8String:s];
  NSLog(@"%@", str);
}