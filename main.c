#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <SDL.h>
#include <SDL_ttf.h>

static void (*sdl_funcs[]) = {
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
SDL_PumpEvents,
SDL_GetMouseState,
SDL_GetKeyboardState,
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
TTF_RenderUTF8_Blended
};

int main(int argc, char *argv[])
{
	struct stat filestat;
	stat(argv[1], &filestat);
	uint16_t *data = mmap(NULL, filestat.st_size, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0);
	int fd = open(argv[1], O_RDONLY);
	read(fd, data, filestat.st_size);
	void(*boot)(char*[], void*[]) = (void(*)(char*[], void*[]))((char*)data + data[5]);
//	printf("image start address: 0x%llx\n", (unsigned long long)data);
	boot(argv, sdl_funcs);
	return 0;
}
