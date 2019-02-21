#ifndef _WIN64
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#else
#define _CRT_SECURE_NO_WARNINGS
#include <io.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <Windows.h>
#include "Winmmap.h"

#endif

#include <SDL.h>
#include <SDL_ttf.h>

#ifdef _WIN64

__int64 FileSize(const wchar_t* name)
{
    HANDLE hFile = CreateFile(name, GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE)
        return -1; // error condition, could call GetLastError to find out more

    LARGE_INTEGER size;
    if (!GetFileSizeEx(hFile, &size))
    {
        CloseHandle(hFile);
        return -1; // error condition, could call GetLastError to find out more
    }

    CloseHandle(hFile);
    return size.QuadPart;
}

int ReadData(const wchar_t* name, uint16_t *pData, DWORD numBytes)
{
    int rv = 0;
    DWORD bytesRead = 0;

    HANDLE hFile = CreateFile(name, GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE) {
        rv = 0;
    }
    else {
        if (ReadFile(hFile, pData, numBytes, &bytesRead, NULL) && bytesRead == numBytes) {
            rv = 1;
        }
        CloseHandle(hFile);
    }

    return ( rv );
}

__int64 FileSizeA(char* name)
{
    HANDLE hFile = CreateFileA(name, GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE)
        return -1; // error condition, could call GetLastError to find out more

    LARGE_INTEGER size;
    if (!GetFileSizeEx(hFile, &size))
    {
        CloseHandle(hFile);
        return -1; // error condition, could call GetLastError to find out more
    }

    CloseHandle(hFile);
    return size.QuadPart;
}

int ReadDataA(char* name, uint16_t *pData, DWORD numBytes)
{
    int rv = 0;
    DWORD bytesRead = 0;

    HANDLE hFile = CreateFileA(name, GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE) {
        rv = 0;
    }
    else {
        if (ReadFile(hFile, pData, numBytes, &bytesRead, NULL) && bytesRead == numBytes) {
            rv = 1;
        }
        CloseHandle(hFile);
    }

    return (rv);
}
#endif

enum
{
	file_open_read,
	file_open_write,
	file_open_readwrite
};

#ifdef _WIN64
void *mgbmmap(void *start, size_t length, int prot, int flags, int fd, off_t offset)
{
    if (fd != -1) {
        int loop = 45;
    }

    void *rv = VirtualAlloc(start, length, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    // 
    return (rv);
}

int
mgbunmap(void *start, size_t length)
{
    return VirtualFree(start, 0, MEM_RELEASE);
}


// windows replacements
int ftruncate(int fd, off_t length)
{
    return (0);
}

#if _WIN64
int munmap(void *addr, size_t length)
{
    return (mgbunmap(addr, length));
}

void *mmap(void *addr, size_t length, int prot, int flags,
    int fd, off_t offset)
{
    return (mgbmmap(addr, length, prot, flags, fd, offset));
}
#endif

#endif

int myopen(char *path, int mode)
{
#ifndef _WIN64
	switch (mode)
	{
		case file_open_read: return open(path, O_RDONLY, 0);
		case file_open_write: return open(path, O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
		case file_open_readwrite: return open(path, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
		default: return -1;
	}
#else
    switch (mode)
    {
        case file_open_read: 
        {
            return _open(path, O_RDONLY | _O_BINARY, 0);
        }
        break;

        case file_open_write: 
        {
            return _open(path, O_CREAT | O_RDWR | O_TRUNC, 0 /*S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH*/);
        }
        break;

        case file_open_readwrite: 
        {
            return _open(path, O_CREAT | O_RDWR, 0 /*S_IRUSR | S_IWUSR*/);
        }
        break;

        default: 
        {
            return -1;
        }
    }
#endif
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
#ifndef _WIN64
return fcntl(0, F_SETFL, fcntl(0, F_GETFL, 0) | O_NONBLOCK);
#else
    return (0);
#endif
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
#ifndef _WIN64
	switch (mode)
	{
		case mprotect_none: return mprotect(addr, len, 0);
		default: return -1;
	}
#else
    return 0;
#endif
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
#ifdef _WIN64
_close,
#else
close,
#endif
ftruncate,
#ifdef _WIN64
_unlink,
_read,
_write,
#else
unlink,
read,
write,
#endif
mymmap,
munmap,
mymprotect,
gettime,
};



int main(int argc, char *argv[])
{
          uint16_t *data = NULL;

          if (argc > 1) {
#ifndef _WIN64
              stat(argv[1], &fs);
              uint16_t *data = mmap(NULL, fs.st_size, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANON, -1, 0);
              int fd = open(argv[1], O_RDONLY);
              read(fd, data, fs.st_size);
#else
            uint32_t size = (uint32_t)FileSizeA(argv[1]);
            data = (uint16_t*)VirtualAlloc(NULL, size, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);
            ReadDataA(argv[1], data, size);
#endif
              if (data) {
                  void(*boot)(char*[], void*[]) = (uint16_t*)(void(*)(char*[], void*[]))((char*)data + data[5]);

                  //printf("image start address: 0x%llx\n", (unsigned long long)data);
                  boot(argv, host_funcs);
              }
          }
          return 0;
      }

