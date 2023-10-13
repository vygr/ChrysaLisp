#ifdef _WIN64

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
#define _CRT_SECURE_NO_WARNINGS
#define DELTA_EPOCH_IN_MICROSECS 11644473600000000Ui64
#include <time.h>
#include <io.h>
#include <windows.h>
#include <tchar.h>
#include <direct.h>
#include <conio.h>
#undef max

char dirbuf[1024];

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
			_mkdir(dirbuf); //, _S_IREAD | _S_IWRITE
			*p = '/';
		}
	}
}

int64_t pii_open(const char *path, uint64_t mode)
{
	int fd;
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
	return -1;
}

char link_buf[128];
struct stat fs;

int64_t pii_open_shared(const char *path, size_t len)
{
	return (int64_t)CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, len, path);
}

int64_t pii_close_shared(const char *path, int64_t hndl)
{
	if (CloseHandle((HANDLE)hndl)) return 0;
	return -1;
}

int64_t pii_read(int64_t fd, void *addr, size_t len)
{
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
	If a directory name is given, it'll walk
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

struct timeval tv;

int64_t pii_gettime()
{
	gettimeofday(&tv, NULL);
	return (((int64_t)tv.tv_sec * 1000000) + tv.tv_usec);
}

bool run_emu = false;

int64_t pii_mprotect(void *addr, size_t len, uint64_t mode)
{
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
	return -1;
}

void *pii_mmap(size_t len, int64_t fd, uint64_t mode)
{
	switch (mode)
	{
	case mmap_exec:
		if (!run_emu) return VirtualAlloc(0, len, MEM_COMMIT, PAGE_EXECUTE_READ);
	case mmap_data:
		return VirtualAlloc(0, len, MEM_COMMIT, PAGE_READWRITE);
	case mmap_shared:
		return MapViewOfFile((HANDLE)fd, FILE_MAP_ALL_ACCESS, 0, 0, len);
	}
	return (void*)-1;
}

int64_t pii_munmap(void *addr, size_t len, uint64_t mode)
{
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
	return -1;
}

void *pii_flush_icache(void* addr, size_t len)
{
	FlushInstructionCache(GetCurrentProcess(), addr, len);
	return addr;
}

std::random_device rd;
std::mt19937 rng(rd());
std::uniform_int_distribution<int> dist(0, 255);

void pii_random(char* addr, size_t len)
{
	for (int i = 0; i < len; ++i) addr[i] = dist(rng);
}

void pii_sleep(uint64_t usec)
{
	uint64_t delay = std::max(usec, static_cast<uint64_t>(1));
	std::this_thread::sleep_for(std::chrono::microseconds(delay));
}

uint64_t pii_close(uint64_t fd)
{
	return close((int)fd);
}

uint64_t pii_unlink(const char *path)
{
	return unlink(path);
}

void (*host_os_funcs[]) = {
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

#endif
