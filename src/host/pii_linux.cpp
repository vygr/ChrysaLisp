#ifdef __linux__

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
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
#include <dirent.h>

static char pii_path_buf[4096];
static char pii_rmdir_buf[4096];

int64_t pii_dirlist(const char *path, char *buf, size_t buf_len)
{
	DIR *dir = opendir(path);
	if (dir == NULL) return 0;

	int64_t total_len = 0;
	struct dirent *entry;
	while ((entry = readdir(dir)) != NULL)
	{
		size_t len = strlen(entry->d_name);
		size_t entry_len = len + 3; // name,type,

		if (buf && total_len + entry_len <= buf_len)
		{
			memcpy(buf + total_len, entry->d_name, len);
			buf[total_len + len] = ',';
			buf[total_len + len + 1] = entry->d_type + '0';
			buf[total_len + len + 2] = ',';
		}
		total_len += entry_len;
	}
	closedir(dir);
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
		if(*p == '/')
		{
			*p = 0;
			mkdir(pii_rmdir_buf, S_IRWXU);
			*p = '/';
		}
	}
}

int64_t pii_open(const char *path, uint64_t mode)
{
	int fd;
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
	return -1;
}

char link_buf[128];
static struct stat pii_stat_fs;

int64_t pii_open_shared(const char *path, size_t len)
{
	strcpy(&link_buf[0], "/tmp/");
	strcpy(&link_buf[5], path);
	int hndl = open(link_buf, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);
	if (hndl == -1)
	{
		while (1)
		{
			if (stat(link_buf, &pii_stat_fs) == 0 && pii_stat_fs.st_size == (off_t)len) break;
			std::this_thread::yield();
		}
		hndl = open(link_buf, O_RDWR, S_IRUSR | S_IWUSR);
	}
	else if (ftruncate(hndl, len) == -1) return -1;
	return hndl;
}

int64_t pii_close_shared(const char *path, int64_t hndl)
{
	close((int)hndl);
	return unlink(path);
}

int64_t pii_read(int64_t fd, void *addr, size_t len)
{
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
	if (stat(path, &pii_stat_fs) != 0) return -1;
	st->mtime = pii_stat_fs.st_mtime;
	st->fsize = pii_stat_fs.st_size;
	st->mode = pii_stat_fs.st_mode;
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
#define MAX_DIR_DEPTH 64

struct WalkState {
	DIR* dir;
	size_t path_len;
};

static WalkState walk_stack[MAX_DIR_DEPTH];

int walk_directory(char* path,
		int (*filevisitor)(const char*),
		int (*foldervisitor)(const char *, int))
{
	int stack_ptr = 0;
	DIR* d = opendir(path);
	if (!d) return -1;
	
	size_t initial_len = strlen(path);
	if (foldervisitor(path, FOLDER_PRE))
	{
		closedir(d);
		return -1;
	}
	
	walk_stack[stack_ptr].dir = d;
	walk_stack[stack_ptr].path_len = initial_len;
	stack_ptr++;
	
	while (stack_ptr > 0)
	{
		WalkState* s = &walk_stack[stack_ptr - 1];
		struct dirent* ent = readdir(s->dir);
		
		if (!ent)
		{
			closedir(s->dir);
			path[s->path_len] = '\0';
			int res = foldervisitor(path, FOLDER_POST);
			stack_ptr--;
			if (res && stack_ptr > 0)
			{
				while (stack_ptr > 0) closedir(walk_stack[--stack_ptr].dir);
				return -1;
			}
			continue;
		}
		
		if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0) continue;
		
		path[s->path_len] = '/';
		strcpy(path + s->path_len + 1, ent->d_name);
		
		if (ent->d_type == DT_DIR)
		{
			if (stack_ptr >= MAX_DIR_DEPTH)
			{
				while (stack_ptr > 0) closedir(walk_stack[--stack_ptr].dir);
				return -1;
			}
			DIR* sub = opendir(path);
			if (sub)
			{
				if (foldervisitor(path, FOLDER_PRE))
				{
					closedir(sub);
					while (stack_ptr > 0) closedir(walk_stack[--stack_ptr].dir);
					return -1;
				}
				walk_stack[stack_ptr].dir = sub;
				walk_stack[stack_ptr].path_len = strlen(path);
				stack_ptr++;
			}
		}
		else
		{
			if (filevisitor(path))
			{
				while (stack_ptr > 0) closedir(walk_stack[--stack_ptr].dir);
				return -1;
			}
		}
	}
	return 0;
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
	if(stat(fqname, &pii_stat_fs) == 0)
	{
		if(S_ISDIR(pii_stat_fs.st_mode) != 0 )
		{
			size_t len = strlen(fqname);
			if (len >= sizeof(pii_path_buf)) return -1;
			memcpy(pii_path_buf, fqname, len + 1);
			return walk_directory(pii_path_buf, file_visit_remove, folder_visit_remove);
		}
		else if (S_ISREG(pii_stat_fs.st_mode) != 0)
		{
			return unlink(fqname);
		}
	}
	return -1;
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
	switch (mode)
	{
	case mmap_exec:
		if (!run_emu) return mprotect(addr, len, PROT_READ | PROT_EXEC);
	case mmap_data:
		return mprotect(addr, len, PROT_READ | PROT_WRITE);
	case mmap_none:
		return mprotect(addr, len, PROT_NONE);
	}
	return -1;
}

void *pii_mmap(size_t len, int64_t fd, uint64_t mode)
{
	switch (mode)
	{
	case mmap_exec:
		if (!run_emu) return mmap(0, len, PROT_READ | PROT_EXEC, MAP_PRIVATE | MAP_ANON, (int)fd, 0);
	case mmap_data:
		return mmap(0, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, (int)fd, 0);
	case mmap_shared:
		return mmap(0, len, PROT_READ | PROT_WRITE, MAP_SHARED, (int)fd, 0);
	}
	return (void*)-1;
}

int64_t pii_munmap(void *addr, size_t len, uint64_t mode)
{
	switch (mode)
	{
	case mmap_data:
	case mmap_exec:
	case mmap_shared:
		return munmap(addr, len);
	}
	return -1;
}

void *pii_flush_icache(void* addr, size_t len)
{
	__builtin___clear_cache(addr, ((char*)addr + len));
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
