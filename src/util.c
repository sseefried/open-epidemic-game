#include <stdio.h>
#ifdef ANDROID
#include <android/log.h>
#endif

double float2double(float f) {
  return (double)f;
}

void set_no_buffering() {
  setvbuf(stdout, NULL, _IONBF, 0);
}

#ifdef ANDROID
void androidLog(char *s) {
  __android_log_print(ANDROID_LOG_INFO, "Epidemic", "%s", s);
}
#endif
