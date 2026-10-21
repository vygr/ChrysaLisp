// Android has no shm_open. pii_linux.cpp is built with this put in front of
// it, and the two are files in a folder of the app's own, see android_shm.cpp

#ifndef ANDROID_SHM_H
#define ANDROID_SHM_H
#if defined(__ANDROID__)

#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif
int shm_open(const char *name, int oflag, mode_t mode);
int shm_unlink(const char *name);
#ifdef __cplusplus
}
#endif

#endif
#endif
