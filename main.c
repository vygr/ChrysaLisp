#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#ifdef _WIN64
#define _CRT_SECURE_NO_WARNINGS
#include <io.h>
#include <windows.h>
#else
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
#endif

#include <SDL.h>
#include <SDL_ttf.h>

enum
{
	file_open_read,
	file_open_write,
	file_open_readwrite
};

long long myopen(char *path, int mode)
{
#ifdef _WIN64
	switch (mode)
	{
	case file_open_read: return open(path, O_RDONLY | O_BINARY);
	case file_open_write: return open(path, O_CREAT | O_RDWR | O_BINARY | O_TRUNC, _S_IREAD | _S_IWRITE);
	case file_open_readwrite: return open(path, O_CREAT | O_RDWR | O_BINARY);
	}
#else
	switch (mode)
	{
	case file_open_read: return open(path, O_RDONLY, 0);
	case file_open_write: return open(path, O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	case file_open_readwrite: return open(path, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
	}
#endif
	return -1;
}

long long myread(int fd, void *addr, size_t len)
{
	return read(fd, addr, len);
}

long long mywrite(int fd, void *addr, size_t len)
{
	return write(fd, addr, len);
}

#ifdef _WIN64
long long ftruncate(int fd, off_t length)
{
	return 0;
}
#endif

struct stat fs;
struct finfo
{
	long long mtime;
	long long fsize;
	unsigned short mode;
};

long long mystat(char *path, struct finfo *st)
{
	if (stat(path, &fs) != 0) return -1;
	st->mtime = fs.st_mtime;
	st->fsize = fs.st_size;
	st->mode = fs.st_mode;
	return 0;
}

long long noneblk()
{
#ifndef _WIN64
	return fcntl(0, F_SETFL, fcntl(0, F_GETFL, 0) | O_NONBLOCK);
#else
    return 0;
#endif
}

#ifdef _WIN64
int gettimeofday(struct timeval *tp, struct timezone *tzp)
{
	// Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
	// This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
	// until 00:00:00 January 1, 1970 
	static const uint64_t EPOCH = ((uint64_t)116444736000000000ULL);

	SYSTEMTIME  system_time;
	FILETIME    file_time;
	uint64_t    time;

	GetSystemTime(&system_time);
	SystemTimeToFileTime(&system_time, &file_time);
	time = ((uint64_t)file_time.dwLowDateTime);
	time += ((uint64_t)file_time.dwHighDateTime) << 32;

	tp->tv_sec = (long)((time - EPOCH) / 10000000L);
	tp->tv_usec = (long)(system_time.wMilliseconds * 1000);
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

void *mymmap(void *addr, size_t len, int mode, int fd, off_t pos)
{
#ifdef _WIN64
	switch (mode)
	{
	case mmap_data: return VirtualAlloc(addr, len, MEM_COMMIT, PAGE_READWRITE);
	case mmap_exec: return VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	case mmap_shared: return VirtualAlloc(addr, len, MEM_COMMIT, PAGE_READWRITE);
	}
#else
	switch (mode)
	{
	case mmap_data: return mmap(addr, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, fd, pos);
	case mmap_exec: return mmap(addr, len, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANON, fd, pos);
	case mmap_shared: return mmap(addr, len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, pos);
    }
#endif
	return 0;
}

long long mymunmap(void *addr, size_t len)
{
#ifdef _WIN64
	if (VirtualFree(addr, len, MEM_RELEASE)) return 0;
	return -1;
#else
	return munmap(addr, len);
#endif
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
TTF_Init,
TTF_Quit,
TTF_OpenFont,
TTF_CloseFont,
TTF_SizeUTF8,
TTF_FontAscent,
TTF_FontDescent,
TTF_FontHeight,
TTF_RenderUTF8_Blended,

exit,
noneblk,
mystat,
myopen,
close,
ftruncate,
unlink,
myread,
mywrite,
mymmap,
mymunmap,
mymprotect,
gettime,
};

int main(int argc, char *argv[])
{
	if (argc > 1)
	{
		long long fd = myopen(argv[1], file_open_read);
		if (fd != -1)
		{
			stat(argv[1], &fs);
			uint16_t *data = mymmap(NULL, fs.st_size, mmap_exec, -1, 0);
			if (data)
			{
				read(fd, data, fs.st_size);
				void(*boot)(char*[], void*[]) = (void(*)(char*[], void*[]))((char*)data + data[5]);
				//printf("image start address: 0x%llx\n", (unsigned long long)data);
				boot(argv, host_funcs);
			}
			close(fd);
		}
	}
	return 0;
}
