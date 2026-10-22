// shm_open and shm_unlink for Android, which has neither. A name is a file in
// tmp/shm of the folder the app runs in, the one mobile_main.cpp goes to.
// It is mapped as shared memory is, and lasts till it is unlinked.

#if defined(__ANDROID__)

#include "android_shm.h"
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

static void shm_path(const char *name, char *buf, size_t len)
{
	while (*name == '/') ++name;
	mkdir("tmp", 0700);
	mkdir("tmp/shm", 0700);
	snprintf(buf, len, "tmp/shm/%s", name);
}

extern "C" int shm_open(const char *name, int oflag, mode_t mode)
{
	char path[256];
	shm_path(name, path, sizeof(path));
	return open(path, oflag | O_CLOEXEC, mode);
}

extern "C" int shm_unlink(const char *name)
{
	char path[256];
	shm_path(name, path, sizeof(path));
	return unlink(path);
}

#endif
