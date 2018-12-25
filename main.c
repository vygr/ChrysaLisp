#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <SDL.h>
#include <SDL_ttf.h>

long long gettime()
{
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return tv.tv_sec * 1000000 + tv.tv_usec;
}

struct stat fs;

struct mystat
{
	long long mtime;
	long long fsize;
	unsigned short mode;
};

long long mystat(char *path, struct mystat *st)
{
	if (stat(path, &fs) != 0) return -1;
	st->mtime = fs.st_mtime;
	st->fsize = fs.st_size;
	st->mode = fs.st_mode;
	return 0;
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

open,
close,
read,
write,
ftruncate,
unlink,
fcntl,
mmap,
munmap,
mprotect,
exit,
mystat,
gettime
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
