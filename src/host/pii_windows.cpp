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
#include <stdlib.h>
#define _CRT_SECURE_NO_WARNINGS
#define DELTA_EPOCH_IN_MICROSECS 11644473600000000ULL
#include <time.h>
#include <io.h>
#include <windows.h>
#include <tchar.h>
#include <direct.h>
#include <conio.h>
#undef max

static char pii_path_buf[4096];
static char pii_rmdir_buf[4096];
static char pii_win_buf[4096];

int64_t pii_dirlist(const char *path, char *buf, size_t buf_len)
{
	size_t path_len = strlen(path);
	size_t cwd_len = GetCurrentDirectoryA(sizeof(pii_win_buf), pii_win_buf);
	if (cwd_len == 0 || cwd_len + path_len + 3 >= sizeof(pii_win_buf)) return 0;

	pii_win_buf[cwd_len++] = '\\';
	memcpy(pii_win_buf + cwd_len, path, path_len);
	cwd_len += path_len;
	pii_win_buf[cwd_len++] = '\\';
	pii_win_buf[cwd_len++] = '*';
	pii_win_buf[cwd_len++] = 0;

    // Made static to keep it off the tiny 8KB ChrysaLisp stack
	static WIN32_FIND_DATAA FindData;
	HANDLE hFind = FindFirstFileA(pii_win_buf, &FindData);
	if (hFind == INVALID_HANDLE_VALUE) return 0;

	int64_t total_len = 0;
	do
	{
		size_t len = strlen(FindData.cFileName);
		size_t entry_len = len + 3; // name,type,

		if (buf && total_len + entry_len <= buf_len)
		{
			memcpy(buf + total_len, FindData.cFileName, len);
			buf[total_len + len] = ',';
			if (FindData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
			{
				buf[total_len + len + 1] = '4';
			}
			else buf[total_len + len + 1] = '8';
			buf[total_len + len + 2] = ',';
		}
		total_len += entry_len;
	} while (FindNextFileA(hFind, &FindData) != 0);
	FindClose(hFind);
	return total_len;
}

static void rmkdir(const char *path)
{
	char *p = NULL;
	size_t len = strlen(path);
	if (len >= sizeof(pii_rmdir_buf)) return;
	memcpy(pii_rmdir_buf, path, len + 1);
	for (p = pii_rmdir_buf + 1; *p; p++)
	{
		if(*p == '/' || *p == '\\')
		{
			char old = *p;
			*p = 0;
			_mkdir(pii_rmdir_buf);
			*p = old;
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
static struct stat pii_stat_fs;

int64_t pii_open_shared(const char *path, size_t len)
{
	return (int64_t)CreateFileMappingA(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, (DWORD)len, path);
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
		if (!_kbhit()) return -1;
		int ch = _getch();
		putchar(ch);
		if (ch == 13) putchar(10);
		if (ch == 8) putchar(32), putchar(8);
		*((char*)addr) = (char)ch;
		return 1;
	}
	return read((int)fd, addr, (unsigned int)len);
}

int64_t pii_write(int64_t fd, void *addr, size_t len)
{
	return write((int)fd, addr, (unsigned int)len);
}

int64_t pii_seek(int64_t fd, int64_t pos, unsigned char offset)
{
	return (lseek((int)fd, (long)pos, offset));
}

int64_t pii_stat(const char *path, struct pii_stat_info *st)
{
	if (stat(path, &pii_stat_fs) != 0) return -1;
	st->mtime = pii_stat_fs.st_mtime;
	st->fsize = pii_stat_fs.st_size;
	st->mode = pii_stat_fs.st_mode;
	return 0;
}

#define FOLDER_PRE 0
#define FOLDER_POST 1
#define MAX_DIR_DEPTH 64

struct WalkState {
	HANDLE hFind;
	size_t path_len;
};

static WalkState walk_stack[MAX_DIR_DEPTH];

int walk_directory(char* path, size_t max_len,
	int (*filevisitor)(const char*),
	int (*foldervisitor)(const char*, int))
{
	int stack_ptr = 0;
	size_t initial_len = strlen(path);
	
	if (initial_len + 5 >= 4096) return -1;
	
	strcpy(path + initial_len, "\\*.*");
    // Made static to keep it off the tiny 8KB stack
	static WIN32_FIND_DATAA fd;
	HANDLE h = FindFirstFileA(path, &fd);
	path[initial_len] = '\0';
	
	if (h == INVALID_HANDLE_VALUE) return -1;
	
	if (foldervisitor(path, FOLDER_PRE))
	{
		FindClose(h);
		return -1;
	}
	
	walk_stack[stack_ptr].hFind = h;
	walk_stack[stack_ptr].path_len = initial_len;
	stack_ptr++;
	
	while (stack_ptr > 0)
	{
		WalkState* s = &walk_stack[stack_ptr - 1];
		BOOL found = FindNextFileA(s->hFind, &fd);
		
		if (!found)
		{
			FindClose(s->hFind);
			path[s->path_len] = '\0';
			int res = foldervisitor(path, FOLDER_POST);
			stack_ptr--;
			if (res && stack_ptr > 0)
			{
				while (stack_ptr > 0) FindClose(walk_stack[--stack_ptr].hFind);
				return -1;
			}
			continue;
		}
		
		if (strcmp(fd.cFileName, ".") == 0 || strcmp(fd.cFileName, "..") == 0) continue;
		
		size_t next_len = s->path_len + 1 + strlen(fd.cFileName);
		if (next_len >= max_len) return -1;

		path[s->path_len] = '\\';
		strcpy(path + s->path_len + 1, fd.cFileName);
		
		if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
		{
			if (stack_ptr >= MAX_DIR_DEPTH)
			{
				while (stack_ptr > 0) FindClose(walk_stack[--stack_ptr].hFind);
				return -1;
			}
			
			size_t sub_len = strlen(path);
			if (sub_len + 5 >= 4096)
			{
				while (stack_ptr > 0) FindClose(walk_stack[--stack_ptr].hFind);
				return -1;
			}
			
			if (foldervisitor(path, FOLDER_PRE))
			{
				while (stack_ptr > 0) FindClose(walk_stack[--stack_ptr].hFind);
				return -1;
			}

			strcpy(path + sub_len, "\\*.*");
			HANDLE sub = FindFirstFileA(path, &fd);
			path[sub_len] = '\0';
			
			if (sub != INVALID_HANDLE_VALUE)
			{
				walk_stack[stack_ptr].hFind = sub;
				walk_stack[stack_ptr].path_len = sub_len;
				stack_ptr++;
			}
		}
		else
		{
			if (filevisitor(path))
			{
				while (stack_ptr > 0) FindClose(walk_stack[--stack_ptr].hFind);
				return -1;
			}
		}
	}
	return 0;
}

int file_visit_remove(const char *fname)
{
	return unlink(fname);
}

int folder_visit_remove(const char *fname, int state)
{
	return ( state == FOLDER_PRE ) ? 0 : rmdir(fname);
}

int64_t pii_remove(const char *fqname)
{
	if(stat(fqname, &pii_stat_fs) == 0)
	{
		if(S_ISDIR(pii_stat_fs.st_mode) != 0 )
		{
			size_t len = strlen(fqname);
			if (len >= sizeof(pii_path_buf)) return -1;
			memcpy(pii_path_buf, fqname, len + 1);
		return walk_directory(pii_path_buf, sizeof(pii_path_buf), file_visit_remove, folder_visit_remove);
		}
		else if (S_ISREG(pii_stat_fs.st_mode) != 0)
		{
			return unlink(fqname);
		}
	}
	return -1;
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

void pii_random(char* addr, size_t len)
{
	static bool seeded = false;
	if (!seeded) {
        // XOR Time and PID guarantees concurrent nodes won't generate the same ID
		srand((unsigned int)pii_gettime() ^ (unsigned int)GetCurrentProcessId());
		seeded = true;
	}
	for (size_t i = 0; i < len; ++i) addr[i] = rand() & 0xff;
}

void pii_sleep(uint64_t usec)
{
	if (usec < 300) {
		SwitchToThread();
		return;
	}

    // High precision hybrid spin/sleep for Windows
	static LARGE_INTEGER freq = { 0 };
	if (freq.QuadPart == 0) {
		QueryPerformanceFrequency(&freq);
	}

	LARGE_INTEGER start, current;
	QueryPerformanceCounter(&start);
	uint64_t wait_ticks = (usec * freq.QuadPart) / 1000000;

	while (true) {
		QueryPerformanceCounter(&current);
		uint64_t elapsed = current.QuadPart - start.QuadPart;
		if (elapsed >= wait_ticks) break;

		uint64_t remaining_usec = ((wait_ticks - elapsed) * 1000000) / freq.QuadPart;
		if (remaining_usec > 2000) {
			Sleep(1);
		} else {
			SwitchToThread();
		}
	}
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