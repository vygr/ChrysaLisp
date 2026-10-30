#if defined(_HOST_GUI)
#if _HOST_GUI == 3 || _HOST_GUI == 4

// SDL3 is loaded when the host starts, see sdl3_load.h. The one it was built
// with is tried first, then the one the system finds by name. CL_SDL3 is the
// path of an SDL3 to load in place of those, and only that.

#include <SDL3/SDL.h>
#include <stdlib.h>
#include <string.h>
#include "sdl3_load.h"

#ifdef _WIN64
#include <windows.h>
#define SDL3_NAME "SDL3.dll"
static void *lib_open(const char *path) { return (void*)LoadLibraryA(path); }
static void *lib_find(void *lib, const char *name) { return (void*)GetProcAddress((HMODULE)lib, name); }
#else
#include <dlfcn.h>
#ifdef __APPLE__
#define SDL3_NAME "libSDL3.0.dylib"
#else
#define SDL3_NAME "libSDL3.so.0"
#endif
static void *lib_open(const char *path) { return dlopen(path, RTLD_NOW | RTLD_LOCAL); }
static void *lib_find(void *lib, const char *name) { return dlsym(lib, name); }
#endif

#define SDL3_DEFINE(name) decltype(cl_##name) cl_##name = nullptr;
SDL3_CALLS(SDL3_DEFINE)
#undef SDL3_DEFINE

//the names, one string, each ended by a 0, and where each is to go, in order
#define SDL3_NAME_OF(name) #name "\0"
static const char sdl3_names[] = SDL3_CALLS(SDL3_NAME_OF);
#undef SDL3_NAME_OF
#define SDL3_PLACE_OF(name) (void**)&cl_##name,
static void **const sdl3_places[] = { SDL3_CALLS(SDL3_PLACE_OF) };
#undef SDL3_PLACE_OF

bool host_sdl3_load(void)
{
	void *lib = nullptr;
	const char *path = getenv("CL_SDL3");
	if (path) lib = lib_open(path);
	else
	{
	#ifdef HOST_SDL3_LIB
		lib = lib_open(HOST_SDL3_LIB);
	#endif
		if (!lib) lib = lib_open(SDL3_NAME);
	}
	if (!lib) return false;
	const char *name = sdl3_names;
	for (auto place : sdl3_places)
	{
		if (!(*place = lib_find(lib, name))) return false;
		name += strlen(name) + 1;
	}
	return true;
}

#endif
#endif
