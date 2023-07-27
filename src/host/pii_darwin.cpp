#ifdef __APPLE__

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
#include <libkern/OSCacheControl.h>

char dirbuf[1024];

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
			mkdir(dirbuf, S_IRWXU);
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
struct stat fs;

int64_t pii_open_shared(const char *path, size_t len)
{
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
				snprintf(NulPosition, sizeof(ent->d_name) + 1, "%c%s", slash, ent->d_name);
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
	sys_icache_invalidate(addr, len);
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
