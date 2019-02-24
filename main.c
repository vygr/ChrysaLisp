#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#ifdef _WIN64
#define _CRT_SECURE_NO_WARNINGS
#define DELTA_EPOCH_IN_MICROSECS 11644473600000000Ui64
#include < time.h >
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
			size_t data_size;
			stat(argv[1], &fs);
			data_size = fs.st_size;
			uint16_t *data = mymmap(NULL, data_size, mmap_exec, -1, 0);
			if (data)
			{
				read(fd, data, data_size);
				void(*boot)(char*[], void*[]) = (void(*)(char*[], void*[]))((char*)data + data[5]);
				//printf("image start address: 0x%llx\n", (unsigned long long)data);
				boot(argv, host_funcs);
				mymunmap(data, data_size);
			}
			close(fd);
		}
	}
	return 0;
}
