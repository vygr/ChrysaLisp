#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#ifdef _WIN64
	#define _CRT_SECURE_NO_WARNINGS
	#define DELTA_EPOCH_IN_MICROSECS 11644473600000000Ui64
	#include <time.h>
	#include <io.h>
	#include <windows.h>
	#include <tchar.h>
#else
	#include <sys/mman.h>
	#include <sys/time.h>
	#include <unistd.h>
	#include <dirent.h>
#endif

#include <SDL.h>

enum
{
	file_open_read,
	file_open_write,
	file_open_readwrite
};

char dirbuf[1024];

#ifdef _WIN64
long long mylist_dir(const char *path, char *buf, size_t buf_len)
{
	char *fbuf = NULL;
	size_t fbuf_len = 0;
	size_t path_len = strlen(path);
	size_t cwd_len = GetCurrentDirectory(1024, dirbuf);
	HANDLE hFind;
	WIN32_FIND_DATA FindData;
	dirbuf[cwd_len++] = '\\';
	strcpy(dirbuf + cwd_len, path);
	cwd_len += path_len;
	dirbuf[cwd_len++] = '\\';
	dirbuf[cwd_len++] = '*';
	dirbuf[cwd_len++] = 0;
	hFind = FindFirstFile(dirbuf, &FindData);
	if (hFind == INVALID_HANDLE_VALUE) return 0;
	do
	{
		size_t len = strlen(FindData.cFileName);
		fbuf = realloc(fbuf, fbuf_len + len + 3);
		memcpy(fbuf + fbuf_len, FindData.cFileName, len);
		fbuf_len += len;
		fbuf[fbuf_len++] = ',';
		if (FindData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
		{
			fbuf[fbuf_len++] = '4';
		}
		else fbuf[fbuf_len++] = '8';
		fbuf[fbuf_len++] = ',';
	} while (FindNextFile(hFind, &FindData) != 0);
	FindClose(hFind);
	if (buf) memcpy(buf, fbuf, fbuf_len > buf_len ? buf_len : fbuf_len);
	free(fbuf);
	return fbuf_len;
}
#else
long long mylist_dir(const char *path, char *buf, size_t buf_len)
{
	char *fbuf = NULL;
	size_t fbuf_len = 0;
	struct dirent *entry;
	DIR *dir = opendir(path);
	if (dir == NULL) return 0;
	while ((entry = readdir(dir)) != NULL)
	{
		size_t len = strlen(entry->d_name);
		fbuf = realloc(fbuf, fbuf_len + len + 3);
		memcpy(fbuf + fbuf_len, entry->d_name, len);
		fbuf_len += len;
		fbuf[fbuf_len++] = ',';
		fbuf[fbuf_len++] = entry->d_type + '0';
		fbuf[fbuf_len++] = ',';
	}
	closedir(dir);
	if (buf) memcpy(buf, fbuf, fbuf_len > buf_len ? buf_len : fbuf_len);
	free(fbuf);
	return fbuf_len;
}
#endif

static void rmkdir(const char *path)
{
	char *p = NULL;
	size_t len;
	len = strlen(path);
	memcpy(dirbuf, path, len + 1);
	for (p = dirbuf + 1; *p; p++)
	{
		if(*p == '/')
		{
			*p = 0;
#ifdef _WIN64
			mkdir(dirbuf, _S_IREAD | _S_IWRITE);
#else
			mkdir(dirbuf, S_IRWXU);
#endif
			*p = '/';
		}
	}
}

long long myopen(const char *path, int mode)
{
#ifdef _WIN64
	switch (mode)
	{
	case file_open_read: return open(path, O_RDONLY | O_BINARY);
	case file_open_write:
	{
		int fd;
		fd = open(path, O_CREAT | O_RDWR | O_BINARY | O_TRUNC, _S_IREAD | _S_IWRITE);
		if (fd != -1) return fd;
		rmkdir(path);
		return open(path, O_CREAT | O_RDWR | O_BINARY | O_TRUNC, _S_IREAD | _S_IWRITE);
	}
	case file_open_readwrite: return open(path, O_CREAT | O_RDWR | O_BINARY);
	}
#else
	switch (mode)
	{
	case file_open_read: return open(path, O_RDONLY, 0);
	case file_open_write:
	{
		int fd;
		fd = open(path, O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
		if (fd != -1) return fd;
		rmkdir(path);
		return open(path, O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	}
	case file_open_readwrite: return open(path, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
	}
#endif
	return -1;
}

char link_buf[128];
struct stat fs;

long long myopenshared(const char *path, size_t len)
{
#ifdef _WIN64
	return (long long)CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, len, path);
#else
	strcpy(&link_buf[0], "/tmp/");
	strcpy(&link_buf[5], path);
	int hndl = open(link_buf, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);
	if (hndl == -1)
	{
		while (1)
		{
			stat(link_buf, &fs);
			if (fs.st_size == len) break;
			sleep(0);
		}
		hndl = open(link_buf, O_RDWR, S_IRUSR | S_IWUSR);
	}
	else ftruncate(hndl, len);
	return hndl;
#endif
}

long long mycloseshared(const char *path, long long hndl)
{
#ifdef _WIN64
	if (CloseHandle((HANDLE)hndl)) return 0;
	return -1;
#else
	close(hndl);
	return unlink(path);
#endif
}

long long myread(int fd, void *addr, size_t len)
{
#ifdef _WIN64
	if (!fd)
	{
		if (!kbhit()) return -1;
		int ch = getch();
		putchar(ch);
		if (ch == 13) putchar(10);
		if (ch == 8) putchar(32), putchar(8);
		*((char*)addr) = ch;
		return 1;
	}
#endif
	return read(fd, addr, len);
}

long long mywrite(int fd, void *addr, size_t len)
{
	return write(fd, addr, len);
}

struct finfo
{
	long long mtime;
	long long fsize;
	unsigned short mode;
};

long long mystat(const char *path, struct finfo *st)
{
	if (stat(path, &fs) != 0) return -1;
	st->mtime = fs.st_mtime;
	st->fsize = fs.st_size;
	st->mode = fs.st_mode;
	return 0;
}

#ifdef _WIN64
int gettimeofday(struct timeval *tv, struct timezone *tz)
{
	if (tv != NULL)
	{
		FILETIME ft;
		unsigned __int64 tmpres = 0;
		GetSystemTimePreciseAsFileTime(&ft);
		tmpres |= ft.dwHighDateTime;
		tmpres <<= 32;
		tmpres |= ft.dwLowDateTime;
		tmpres /= 10;
		tmpres -= DELTA_EPOCH_IN_MICROSECS;
		tv->tv_sec = (long)(tmpres / 1000000UL);
		tv->tv_usec = (long)(tmpres % 1000000UL);
	}
	return 0;
}
#endif

struct timeval tv;

long long gettime()
{
	gettimeofday(&tv, NULL);
	return tv.tv_sec * 1000000 + tv.tv_usec;
}

enum
{
	mprotect_none
};

long long mymprotect(void *addr, size_t len, int mode)
{
#ifdef _WIN64
	int old;
	switch (mode)
	{
	case mprotect_none: if (VirtualProtect(addr, len, PAGE_NOACCESS, &old)) return 0;
	}
#else
	switch (mode)
	{
	case mprotect_none: return mprotect(addr, len, 0);
	}
#endif
	return -1;
}

enum
{
	mmap_data,
	mmap_exec,
	mmap_shared
};

void *mymmap(size_t len, long long fd, int mode)
{
#ifdef _WIN64
	switch (mode)
	{
	case mmap_data: return VirtualAlloc(0, len, MEM_COMMIT, PAGE_READWRITE);
	case mmap_exec: return VirtualAlloc(0, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	case mmap_shared: return MapViewOfFile((HANDLE)fd, FILE_MAP_ALL_ACCESS, 0, 0, len);
	}
#else
	switch (mode)
	{
	case mmap_data: return mmap(0, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, fd, 0);
	case mmap_exec: return mmap(0, len, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANON, fd, 0);
	case mmap_shared: return mmap(0, len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	}
#endif
	return 0;
}

long long mymunmap(void *addr, size_t len, int mode)
{
#ifdef _WIN64
	switch (mode)
	{
	case mmap_data:
	case mmap_exec:
	{
		if (VirtualFree(addr, 0, MEM_RELEASE)) return 0;
		break;
	}
	case mmap_shared:
		if (UnmapViewOfFile(addr)) return 0;
		break;
	}
#else
	switch (mode)
	{
	case mmap_data:
	case mmap_exec:
	case mmap_shared:
		return munmap(addr, len);
	}
#endif
	return -1;
}

void *myclearicache(void* addr, size_t len)
{
#ifdef _WIN64
#else
#ifdef __APPLE__
#else
	__clear_cache(addr, addr + len);
#endif
#endif
	return addr;
}

static void (*host_funcs[]) = {
SDL_SetMainReady,
SDL_Init,
SDL_GetError,
SDL_Quit,
SDL_CreateWindow,
SDL_CreateWindowAndRenderer,
SDL_DestroyWindow,
SDL_Delay,
SDL_CreateRenderer,
SDL_SetRenderDrawColor,
SDL_RenderFillRect,
SDL_RenderPresent,
SDL_RenderSetClipRect,
SDL_SetRenderDrawBlendMode,
SDL_PollEvent,
SDL_RenderDrawRect,
SDL_FreeSurface,
SDL_CreateTextureFromSurface,
SDL_DestroyTexture,
SDL_RenderCopy,
SDL_SetTextureBlendMode,
SDL_SetTextureColorMod,
SDL_CreateRGBSurfaceFrom,
SDL_ComposeCustomBlendMode,
SDL_CreateTexture,
SDL_SetRenderTarget,
SDL_RenderClear,

exit,
mystat,
myopen,
close,
unlink,
myread,
mywrite,
mymmap,
mymunmap,
mymprotect,
gettime,
myopenshared,
mycloseshared,
myclearicache,
mylist_dir
};

int main(int argc, char *argv[])
{
	int ret_val = 0;
	if (argc > 1)
	{
		long long fd = myopen(argv[1], file_open_read);
		if (fd != -1)
		{
			stat(argv[1], &fs);
			size_t data_size = fs.st_size;
			uint16_t *data = mymmap(data_size, -1, mmap_exec);
			if (data)
			{
				read(fd, data, data_size);
				myclearicache(data, data_size);
				//printf("image start address: 0x%llx\n", (unsigned long long)data);
#ifndef _WIN64
				fcntl(0, F_SETFL, fcntl(0, F_GETFL, 0) | O_NONBLOCK);
#endif
				ret_val = ((int(*)(char*[], void*[]))((char*)data + data[5]))(argv, host_funcs);
				mymunmap(data, data_size, mmap_exec);
			}
			close(fd);
		}
	}
	return ret_val;
}
