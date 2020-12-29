#define _CRT_INTERNAL_NONSTDC_NAMES 1
#include <sys/stat.h>
#if !defined(S_ISREG) && defined(S_IFMT) && defined(S_IFREG)
	#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif
#if !defined(S_ISDIR) && defined(S_IFMT) && defined(S_IFDIR)
	#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif

#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <random>
#include <thread>
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
#ifndef MAP_JIT
	#define MAP_JIT 0
#endif

enum
{
	file_open_read,
	file_open_write,
	file_open_append
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

long long myopen(const char *path, int mode)
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
	else if (ftruncate(hndl, len) == -1) return -1;
	return hndl;
#endif
}

long long mycloseshared(const char *path, long long hndl)
{
#ifdef _WIN64
	if (CloseHandle((HANDLE)hndl)) return 0;
	return -1;
#else
	close((int)hndl);
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

long long myseek(long long fd, long long pos, unsigned char offset)
{
	return (lseek((int)fd, pos, offset));
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
	// Natural close
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
	long long myremove(const char *fqname) -> 0 | -1
	Will remove a file or a directory
	If a directory name is given, it will walk
	the directory and remove all files and
	subdirectories in it's path
*/
long long myremove(const char *fqname)
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

long long gettime()
{
	gettimeofday(&tv, NULL);
	return (((long long)tv.tv_sec * 1000000) + tv.tv_usec);
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
	case mprotect_none: if (VirtualProtect(addr, len, PAGE_NOACCESS, (PDWORD)&old)) return 0;
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
	case mmap_data: return mmap(0, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, (int)fd, 0);
	case mmap_exec: return mmap(0, len, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANON | MAP_JIT, (int)fd, 0);
	case mmap_shared: return mmap(0, len, PROT_READ | PROT_WRITE, MAP_SHARED, (int)fd, 0);
	}
#endif
	return (void*)-1;
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
		sys_icache_invalidate(addr, len);
	#else
		__clear_cache(addr, ((char*)addr + len));
	#endif
#endif
	return addr;
}

std::random_device rd;
std::uniform_int_distribution<int> dist(0, 256);
void myrand(char* addr, size_t len)
{
	for (int i = 0; i < len; ++i) addr[i] = dist(rd);
}

void mysleep(uint64_t ms)
{
	std::this_thread::sleep_for(std::chrono::milliseconds(ms));
}

static void (*host_os_funcs[]) = {
(void*)exit,
(void*)mystat,
(void*)myopen,
(void*)close,
(void*)unlink,
(void*)myread,
(void*)mywrite,
(void*)mymmap,
(void*)mymunmap,
(void*)mymprotect,
(void*)gettime,
(void*)myopenshared,
(void*)mycloseshared,
(void*)myclearicache,
(void*)mylist_dir,
(void*)myremove,
(void*)myseek,
(void*)myrand,
(void*)mysleep,
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

#define VP64_STACK_SIZE 8192
int vp64(uint8_t* data, int64_t *stack, int64_t *argv, int64_t *host_os_funcs, int64_t *host_gui_funcs);

int main(int argc, char *argv[])
{
	int ret_val = 0;
	if (argc > 1)
	{
		//check for -e option
		bool run_emu = false;
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

		long long fd = myopen(argv[1], file_open_read);
		if (fd != -1)
		{
			stat(argv[1], &fs);
			size_t data_size = fs.st_size;
			uint16_t *data = (uint16_t*)mymmap(data_size, -1, mmap_exec);
			if (data != (uint16_t*)-1)
			{
				if (read((int)fd, data, data_size) == data_size)
				{
					myclearicache(data, data_size);
					//printf("image start address: 0x%llx\n", (unsigned long long)data);
	#ifndef _WIN64
					fcntl(0, F_SETFL, fcntl(0, F_GETFL, 0) | O_NONBLOCK);
	#endif
					if (run_emu)
					{
						int64_t* stack = (int64_t*)mymmap(VP64_STACK_SIZE, -1, mmap_data);
						if (stack)
						{
							printf("ChrysaLisp vp64 emulator v0.1\n");
#ifdef _GUI
							ret_val = vp64((uint8_t*)data, (int64_t*)((char*)stack + VP64_STACK_SIZE), (int64_t*)argv, (int64_t*)host_os_funcs, (int64_t*)host_gui_funcs);
#else
							ret_val = vp64((uint8_t*)data, (int64_t*)((char*)stack + VP64_STACK_SIZE), (int64_t*)argv, (int64_t*)host_os_funcs, (int64_t*)nullptr);
#endif
							mymunmap(stack, VP64_STACK_SIZE, mmap_data);
						}
					}
					else
					{
#ifdef _GUI
						ret_val = ((int(*)(char* [], void* [], void* []))((char*)data + data[5]))(argv, host_os_funcs, host_gui_funcs);
#else
						ret_val = ((int(*)(char* [], void* [], void* []))((char*)data + data[5]))(argv, host_os_funcs, nullptr);
#endif
					}
					mymunmap(data, data_size, mmap_exec);
				}
				else
				{
					printf("Error, failed reading boot_image!\n");
				}
			}
			else
			{
				printf("Error, READ/WRITE/EXEC pages failed!\n");
			}
			close((int)fd);
		}
		else
		{
			printf("Error, boot_image not found!\n");
		}
	}
	else
	{
		printf("Error, no boot_image arg!\n");
	}
	return ret_val;
}
