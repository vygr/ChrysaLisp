#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <SDL.h>
#include <SDL_ttf.h>

enum
{
	file_open_read,
	file_open_write,
	file_open_readwrite
};

int myopen(char *path, int mode)
{
	switch (mode)
	{
		case file_open_read: return open(path, O_RDONLY, 0);
		case file_open_write: return open(path, O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
		case file_open_readwrite: return open(path, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
		default: return -1;
	}
}

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
	return fcntl(0, F_SETFL, fcntl(0, F_GETFL, 0) | O_NONBLOCK);
}

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

int mymprotect(void *addr, size_t len, int mode)
{
	switch (mode)
	{
		case mprotect_none: return mprotect(addr, len, 0);
		default: return -1;
	}
}

enum
{
	mmap_data,
	mmap_exec,
	mmap_shared
};

void *mymmap(void *addr, size_t len, int mode, int fd, off_t pos)
{
	switch (mode)
	{
		case mmap_data: return mmap(addr, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, fd, pos);
		case mmap_exec: return mmap(addr, len, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANON, fd, pos);
		case mmap_shared: return mmap(addr, len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, pos);
		default: return 0;
	}
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
read,
write,
mymmap,
munmap,
mymprotect,
gettime,
};

int main(int argc, char *argv[])
{
	stat(argv[1], &fs);
	uint16_t *data = mmap(NULL, fs.st_size, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0);
	int fd = open(argv[1], O_RDONLY);
	read(fd, data, fs.st_size);
	void(*boot)(char*[], void*[]) = (void(*)(char*[], void*[]))((char*)data + data[5]);
//	printf("image start address: 0x%llx\n", (unsigned long long)data);
	boot(argv, host_funcs);
	return 0;
}
