#define _CRT_INTERNAL_NONSTDC_NAMES 1
#include <sys/stat.h>
#if !defined(S_ISREG) && defined(S_IFMT) && defined(S_IFREG)
	#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif
#if !defined(S_ISDIR) && defined(S_IFMT) && defined(S_IFDIR)
	#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif

#include "pii.h"
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <random>
#include <thread>
#include <iostream>
#ifdef _WIN64
	#define _CRT_SECURE_NO_WARNINGS
	#define DELTA_EPOCH_IN_MICROSECS 11644473600000000Ui64
	#include <time.h>
	#include <io.h>
	#include <windows.h>
	#include <tchar.h>
	#include <direct.h>
	#include <conio.h>
#else
	#include <sys/mman.h>
	#include <sys/time.h>
	#include <unistd.h>
	#include <dirent.h>
#endif
#ifdef __APPLE__
	#include <libkern/OSCacheControl.h>
#endif
#ifdef _GUI
	#include <SDL.h>
#endif

#define VP64_STACK_SIZE 8192
int vp64(uint8_t* data, int64_t *stack, int64_t *argv, int64_t *host_os_funcs, int64_t *host_gui_funcs);
bool run_emu = false;

char dirbuf[1024];

#ifdef _WIN64
int64_t pii_dirlist(const char *path, char *buf, size_t buf_len)
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
		fbuf = (char *)realloc(fbuf, fbuf_len + len + 3);
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
int64_t pii_dirlist(const char *path, char *buf, size_t buf_len)
{
	char *fbuf = NULL;
	size_t fbuf_len = 0;
	struct dirent *entry;
	DIR *dir = opendir(path);
	if (dir == NULL) return 0;
	while ((entry = readdir(dir)) != NULL)
	{
		size_t len = strlen(entry->d_name);
		fbuf = (char*)realloc(fbuf, fbuf_len + len + 3);
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
			_mkdir(dirbuf); //, _S_IREAD | _S_IWRITE
#else
			mkdir(dirbuf, S_IRWXU);
#endif
			*p = '/';
		}
	}
}

int64_t pii_open(const char *path, uint64_t mode)
{
	int fd;
#ifdef _WIN64
	switch (mode)
	{
	case file_open_read: return open(path, O_RDONLY | O_BINARY);
	case file_open_write:
	{
		fd = open(path, O_CREAT | O_RDWR | O_BINARY | O_TRUNC, _S_IREAD | _S_IWRITE);
		if (fd != -1) return fd;
		rmkdir(path);
		return open(path, O_CREAT | O_RDWR | O_BINARY | O_TRUNC, _S_IREAD | _S_IWRITE);
	}
	case file_open_append:
	{
		fd = open(path, O_CREAT | O_RDWR | O_BINARY, _S_IREAD | _S_IWRITE);
		if (fd != -1)
		{
			lseek(fd, 0, SEEK_END);
			return fd;
		}
		else
		{
			rmkdir(path);
			fd = open(path, O_CREAT | O_RDWR | O_BINARY, _S_IREAD | _S_IWRITE);
			if (fd != -1) return fd;
			lseek(fd, 0, SEEK_END);
		}
		return fd;
	}
	}
#else
	switch (mode)
	{
	case file_open_read: return open(path, O_RDONLY, 0);
	case file_open_write:
	{
		fd = open(path, O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
		if (fd != -1) return fd;
		rmkdir(path);
		return open(path, O_CREAT | O_RDWR | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	}
	case file_open_append:
	{
		fd = open(path, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
		if (fd != -1)
		{
			lseek(fd, 0, SEEK_END);
			return fd;
		}
		else
		{
			rmkdir(path);
			fd = open(path, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
			if (fd != -1) return fd;
			lseek(fd, 0, SEEK_END);
		}
		return fd;
	}
	}
#endif
	return -1;
}

char link_buf[128];
struct stat fs;

int64_t pii_open_shared(const char *path, size_t len)
{
#ifdef _WIN64
	return (int64_t)CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, len, path);
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
	else if (ftruncate(hndl, len) == -1) return -1;
	return hndl;
#endif
}

int64_t pii_close_shared(const char *path, int64_t hndl)
{
#ifdef _WIN64
	if (CloseHandle((HANDLE)hndl)) return 0;
	return -1;
#else
	close((int)hndl);
	return unlink(path);
#endif
}

int64_t pii_read(int64_t fd, void *addr, size_t len)
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
	return read((int)fd, addr, len);
}

int64_t pii_write(int64_t fd, void *addr, size_t len)
{
	return write((int)fd, addr, len);
}

int64_t pii_seek(int64_t fd, int64_t pos, unsigned char offset)
{
	return (lseek((int)fd, pos, offset));
}

int64_t pii_stat(const char *path, struct pii_stat_info *st)
{
	if (stat(path, &fs) != 0) return -1;
	st->mtime = fs.st_mtime;
	st->fsize = fs.st_size;
	st->mode = fs.st_mode;
	return 0;
}

/*
	int walk_directory(
		const char *path,
		int (*filevisitor)(const char*),
		int (*foldervisitor)(const char *, int))
	Opens a directory and invokes a visitor (fn) for each entry
*/

#define FOLDER_PRE 0
#define FOLDER_POST 1

#ifdef _WIN64
	// For Chris to consider refactoring the mydirlist logic to use a visitor
	// function
int walk_directory(char* path,
	int (*filevisitor)(const char*),
	int (*foldervisitor)(const char*, int))
{
	char dirpathwild[_MAX_PATH] = { 0 };
	WIN32_FIND_DATAA wfd = { 0 };
	int err = 0;
	sprintf_s(dirpathwild, _MAX_PATH, "%s\\*.*", path);
	HANDLE hFind = FindFirstFileA(dirpathwild, &wfd);
	if (hFind) {
		do {
			if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
				if (strstr(wfd.cFileName, ".") != wfd.cFileName) {
					err = foldervisitor(wfd.cFileName, FOLDER_PRE);
					char buffer[_MAX_PATH] = { 0 };
					sprintf_s(buffer, _MAX_PATH, "%s\\%s\\", path, wfd.cFileName);
					walk_directory(buffer, filevisitor, foldervisitor);

					if (wfd.dwFileAttributes & FILE_ATTRIBUTE_READONLY) {
						err = _chmod(buffer, _S_IWRITE);
					}

					err = foldervisitor(buffer, FOLDER_POST);
				}
			}
			else {
				char buffer[_MAX_PATH] = { 0 };
				sprintf_s(buffer, _MAX_PATH, "%s\\%s", path, wfd.cFileName);

				if (wfd.dwFileAttributes & FILE_ATTRIBUTE_READONLY) {
					err = _chmod(buffer, _S_IWRITE);
				}

				err = filevisitor(buffer);
			}
		} while (FindNextFileA(hFind, &wfd));


		FindClose(hFind);
		err = foldervisitor(path, FOLDER_POST);
	}

	return (1);
}
#else
int walk_directory(char* path,
		int (*filevisitor)(const char*),
		int (*foldervisitor)(const char *, int))
{
	char slash = '/';
	DIR* dir;
	struct dirent *ent;
	char *NulPosition = &path[strlen(path)];
	if ((dir = opendir(path)) != NULL)
	{
		foldervisitor(path, FOLDER_PRE);
		while ((ent = readdir(dir)) != NULL)
		{
			if((strcmp(ent->d_name, ".") != 0) && (strcmp(ent->d_name, "..") != 0))
			{
				sprintf(NulPosition, "%c%s", slash, ent->d_name);
				if (ent->d_type == DT_DIR)
				{
					if (walk_directory(path, filevisitor, foldervisitor))
					{
							closedir(dir);
							return -1;
					}
				}
				else
				{
					if(filevisitor(path))
					{
							closedir(dir);
							return -1;
					}
				}
				*NulPosition = '\0';
			}
		}	 // end while
	} // opendir == NULL
	else
	{
		return -1;
	}
	// Natural pii_close
	closedir(dir);
	return foldervisitor(path, FOLDER_POST);
}
#endif

/*
	int file_visit_remove(const char *fname)
	Removes file being visited
*/
int file_visit_remove(const char *fname)
{
	return unlink(fname);
}

/*
	int folder_visit_remove(const char fname, int state)
	Folder visit both pre-walk and post-walk states
	For post-walk the folder is removed
*/

int folder_visit_remove(const char *fname, int state)
{
	return ( state == FOLDER_PRE ) ? 0 : rmdir(fname);
}

/*
	int64_t pii_remove(const char *fqname) -> 0 | -1
	Will remove a file or a directory
	If a directory name is given, it will walk
	the directory and remove all files and
	subdirectories in it's path
*/
int64_t pii_remove(const char *fqname)
{
	int res = -1;
	if(stat(fqname, &fs) == 0)
	{
		if(S_ISDIR(fs.st_mode) != 0 )
		{
			strcpy(dirbuf, fqname);
			return walk_directory(dirbuf, file_visit_remove, folder_visit_remove);
		}
		else if (S_ISREG(fs.st_mode) != 0)
		{
			return unlink(fqname);
		}
	}
	return res;
}

#ifdef _WIN64
int gettimeofday(struct timeval *tv, struct timezone *tz)
{
	if (tv != NULL)
	{
		FILETIME ft = { 0 };
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

int64_t pii_gettime()
{
	gettimeofday(&tv, NULL);
	return (((int64_t)tv.tv_sec * 1000000) + tv.tv_usec);
}

int64_t pii_mprotect(void *addr, size_t len, uint64_t mode)
{
#ifdef _WIN64
	int old;
	switch (mode)
	{
	case mmap_exec:
		if (!run_emu)
		{
			if (VirtualProtect(addr, len, PAGE_EXECUTE_READ, (PDWORD)&old)) return 0;
			else return -1;
		}
	case mmap_data:
		if (VirtualProtect(addr, len, PAGE_READWRITE, (PDWORD)&old)) return 0;
		else return -1;
	case mmap_none: if (VirtualProtect(addr, len, PAGE_NOACCESS, (PDWORD)&old)) return 0;
	}
#else
	switch (mode)
	{
	case mmap_exec:
		if (!run_emu) return mprotect(addr, len, PROT_READ | PROT_EXEC);
	case mmap_data:
		return mprotect(addr, len, PROT_READ | PROT_WRITE);
	case mmap_none:
		return mprotect(addr, len, PROT_NONE);
	}
#endif
	return -1;
}

void *pii_mmap(size_t len, int64_t fd, uint64_t mode)
{
#ifdef _WIN64
	switch (mode)
	{
	case mmap_exec:
		if (!run_emu) return VirtualAlloc(0, len, MEM_COMMIT, PAGE_EXECUTE_READ);
	case mmap_data:
		return VirtualAlloc(0, len, MEM_COMMIT, PAGE_READWRITE);
	case mmap_shared:
		return MapViewOfFile((HANDLE)fd, FILE_MAP_ALL_ACCESS, 0, 0, len);
	}
#else
	switch (mode)
	{
	case mmap_exec:
		if (!run_emu) return mmap(0, len, PROT_READ | PROT_EXEC, MAP_PRIVATE | MAP_ANON, (int)fd, 0);
	case mmap_data:
		return mmap(0, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, (int)fd, 0);
	case mmap_shared:
		return mmap(0, len, PROT_READ | PROT_WRITE, MAP_SHARED, (int)fd, 0);
	}
#endif
	return (void*)-1;
}

int64_t pii_munmap(void *addr, size_t len, uint64_t mode)
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

void *pii_flush_icache(void* addr, size_t len)
{
#ifdef _WIN64
	FlushInstructionCache(GetCurrentProcess(), addr, len);
#else
	#ifdef __APPLE__
		sys_icache_invalidate(addr, len);
	#else
		__builtin___clear_cache(addr, ((char*)addr + len));
	#endif
#endif
	return addr;
}

std::random_device rd;
std::uniform_int_distribution<int> dist(0, 256);
void pii_random(char* addr, size_t len)
{
	for (int i = 0; i < len; ++i) addr[i] = dist(rd);
}

void pii_sleep(uint64_t usec)
{
	std::this_thread::sleep_for(std::chrono::microseconds(usec));
}

uint64_t pii_close(uint64_t fd)
{
	return close((int)fd);
}

uint64_t pii_unlink(const char *path)
{
	return unlink(path);
}

static void (*host_os_funcs[]) = {
(void*)exit,
(void*)pii_stat,
(void*)pii_open,
(void*)pii_close,
(void*)pii_unlink,
(void*)pii_read,
(void*)pii_write,
(void*)pii_mmap,
(void*)pii_munmap,
(void*)pii_mprotect,
(void*)pii_gettime,
(void*)pii_open_shared,
(void*)pii_close_shared,
(void*)pii_flush_icache,
(void*)pii_dirlist,
(void*)pii_remove,
(void*)pii_seek,
(void*)pii_random,
(void*)pii_sleep,
};

#ifdef _GUI
static void (*host_gui_funcs[]) = {
	(void*)SDL_SetMainReady,
	(void*)SDL_Init,
	(void*)SDL_GetError,
	(void*)SDL_Quit,
	(void*)SDL_CreateWindow,
	(void*)SDL_CreateWindowAndRenderer,
	(void*)SDL_DestroyWindow,
	(void*)SDL_CreateRenderer,
	(void*)SDL_SetRenderDrawColor,
	(void*)SDL_RenderFillRect,
	(void*)SDL_RenderPresent,
	(void*)SDL_RenderSetClipRect,
	(void*)SDL_SetRenderDrawBlendMode,
	(void*)SDL_PollEvent,
	(void*)SDL_RenderDrawRect,
	(void*)SDL_FreeSurface,
	(void*)SDL_CreateTextureFromSurface,
	(void*)SDL_DestroyTexture,
	(void*)SDL_RenderCopy,
	(void*)SDL_SetTextureBlendMode,
	(void*)SDL_SetTextureColorMod,
	(void*)SDL_CreateRGBSurfaceFrom,
	(void*)SDL_ComposeCustomBlendMode,
	(void*)SDL_CreateTexture,
	(void*)SDL_SetRenderTarget,
	(void*)SDL_RenderClear,
};
#endif

int main(int argc, char *argv[])
{
	int ret_val = 0;
	if (argc > 1)
	{
		//check for -e option
		for (int i = 0; i < argc; ++i)
		{
			if (!strcmp(argv[i], "-e"))
			{
				//override boot image to emu image
				run_emu = true;
				argv[1] = (char*)"obj/vp64/VP64/sys/boot_image";
				break;
			}
		}

		int64_t fd = pii_open(argv[1], file_open_read);
		if (fd != -1)
		{
			stat(argv[1], &fs);
			size_t data_size = fs.st_size;
			uint16_t *data = (uint16_t*)pii_mmap(data_size, -1, mmap_data);
			if (data != (uint16_t*)-1)
			{
				if (read((int)fd, data, data_size) == data_size)
				{
					//printf("image start address: 0x%llx\n", (unsigned int64_t)data);
				#ifndef _WIN64
					fcntl(0, F_SETFL, fcntl(0, F_GETFL, 0) | O_NONBLOCK);
				#endif
					if (run_emu)
					{
						int64_t* stack = (int64_t*)pii_mmap(VP64_STACK_SIZE, -1, mmap_data);
						if (stack)
						{
							std::cout << "ChrysaLisp vp64 emulator v0.1" << std::endl;
						#ifdef _GUI
							ret_val = vp64((uint8_t*)data, (int64_t*)((char*)stack + VP64_STACK_SIZE), (int64_t*)argv, (int64_t*)host_os_funcs, (int64_t*)host_gui_funcs);
						#else
							ret_val = vp64((uint8_t*)data, (int64_t*)((char*)stack + VP64_STACK_SIZE), (int64_t*)argv, (int64_t*)host_os_funcs, (int64_t*)nullptr);
						#endif
							pii_munmap(stack, VP64_STACK_SIZE, mmap_data);
						}
					}
					else
					{
						//swap to RX
						pii_flush_icache(data, data_size);
						pii_mprotect(data, data_size, mmap_exec);
					#ifdef _GUI
						ret_val = ((int(*)(char* [], void* [], void* []))((char*)data + data[5]))(argv, host_os_funcs, host_gui_funcs);
					#else
						ret_val = ((int(*)(char* [], void* [], void* []))((char*)data + data[5]))(argv, host_os_funcs, nullptr);
					#endif
					}
					pii_munmap(data, data_size, mmap_exec);
				}
				else std::cout << "Error, failed reading boot_image!" << std::endl;
			}
			else std::cout << "Error, READ/WRITE/EXEC pages failed!" << std::endl;
			pii_close((int)fd);
		}
		else std::cout << "Error, boot_image not found!" << std::endl;
	}
	else std::cout << "Error, no boot_image arg!" << std::endl;
	return ret_val;
}
