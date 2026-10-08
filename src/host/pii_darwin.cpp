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
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
#include <dirent.h>
#include <sched.h>
#include <spawn.h>
#include <signal.h>
#include <errno.h>
#include <libkern/OSCacheControl.h>

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
			sched_yield();
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

// shared memory that is only ever memory, it has no file behind it for the
// system to write back to. It is for pixels that several nodes of this
// machine draw on. One node makes a piece of it under a key, a 64 bit
// number, the others find it by the key, and it lasts till the one that
// made it lets go of the key and the last of them has unmapped it. The key
// is its name, clpx- and the key in hex.
//
// A node that is killed lets go of nothing, and a name is not a file that
// a script can delete. So each name is noted in a file of its own, with the
// pid of who made it, and pii_shm_sweep lets go of the names of the dead.

#define PII_SHM_NOTE "/tmp/chrysalisp_shm_"

static void pii_shm_names(uint64_t key, char *path, char *note)
{
	snprintf(path, 64, "/clpx-%016llx", (unsigned long long)key);
	snprintf(note, 128, PII_SHM_NOTE "clpx-%016llx", (unsigned long long)key);
}

int64_t pii_shm_open(uint64_t key, size_t len, uint64_t create)
{
	// 1 to make it, 0 to find one that is there. Returns a handle for
	// pii_mmap, or -1, and never waits. A key that is taken can not be made
	char path[64], note[128];
	pii_shm_names(key, path, note);
	if (create)
	{
		int fd = shm_open(path, O_CREAT | O_EXCL | O_RDWR, S_IRUSR | S_IWUSR);
		if (fd == -1) return -1;
		if (ftruncate(fd, len) == -1)
		{
			close(fd);
			shm_unlink(path);
			return -1;
		}
		FILE *f = fopen(note, "w");
		if (f)
		{
			fprintf(f, "%d\n", (int)getpid());
			fclose(f);
		}
		return fd;
	}
	int fd = shm_open(path, O_RDWR, 0);
	if (fd == -1) return -1;
	// it is a whole number of pages, so it can be longer than was asked
	struct stat st;
	if (fstat(fd, &st) == -1 || st.st_size < (off_t)len)
	{
		close(fd);
		return -1;
	}
	return fd;
}

int64_t pii_shm_close(uint64_t key, int64_t hndl, uint64_t owner)
{
	// the one that made it lets go of the key as well
	char path[64], note[128];
	close((int)hndl);
	if (owner)
	{
		pii_shm_names(key, path, note);
		shm_unlink(path);
		unlink(note);
	}
	return 0;
}

void pii_shm_sweep()
{
	// let go of every name whose maker is no longer running
	const char *prefix = "chrysalisp_shm_";
	size_t prefix_len = strlen(prefix);
	char path[64], note[512];
	DIR *dir = opendir("/tmp");
	if (!dir) return;
	struct dirent *entry;
	while ((entry = readdir(dir)))
	{
		if (strncmp(entry->d_name, prefix, prefix_len)) continue;
		snprintf(note, sizeof(note), "/tmp/%s", entry->d_name);
		int pid = 0;
		FILE *f = fopen(note, "r");
		if (f)
		{
			if (fscanf(f, "%d", &pid) != 1) pid = 0;
			fclose(f);
		}
		if (pid > 0 && (kill((pid_t)pid, 0) == 0 || errno == EPERM)) continue;
		snprintf(path, sizeof(path), "/%s", entry->d_name + prefix_len);
		shm_unlink(path);
		unlink(note);
	}
	closedir(dir);
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

#define FOLDER_PRE 0
#define FOLDER_POST 1
#define MAX_DIR_DEPTH 64

struct WalkState {
	DIR* dir;
	size_t path_len;
};

static WalkState walk_stack[MAX_DIR_DEPTH];

int walk_directory(char* path, size_t max_len,
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
		
		size_t next_len = s->path_len + 1 + strlen(ent->d_name);
		if (next_len >= max_len) return -1;

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

void pii_random(char* addr, size_t len)
{
	int fd = open("/dev/urandom", O_RDONLY);
	if (fd >= 0) {
		read(fd, addr, len);
		close(fd);
	} else {
		static bool seeded = false;
		if (!seeded) {
			srand((unsigned int)pii_gettime() ^ (unsigned int)getpid());
			seeded = true;
		}
		for (size_t i = 0; i < len; ++i) addr[i] = rand() & 0xff;
	}
}

void pii_sleep(uint64_t usec)
{
	usleep(usec > 0 ? usec : 1);
}

uint64_t pii_close(uint64_t fd)
{
	return close((int)fd);
}

uint64_t pii_unlink(const char *path)
{
	return unlink(path);
}

int64_t pii_sysid(char *buf, size_t len)
{
	if (!buf || len < 16) return -1;

	// 1. Try Linux /etc/machine-id
	int fd = open("/etc/machine-id", O_RDONLY);
	if (fd >= 0) {
		char hex[33];
		ssize_t n = read(fd, hex, 32);
		close(fd);
		if (n == 32) {
			for (size_t i = 0; i < 16; ++i) {
				unsigned int byte_val;
				if (sscanf(&hex[i * 2], "%02x", &byte_val) == 1) {
					buf[i] = (char)byte_val;
				}
			}
			return 0;
		}
	}

	// 2. Fallback: persistent local file .system_id
	fd = open(".system_id", O_RDONLY);
	if (fd >= 0) {
		ssize_t n = read(fd, buf, 16);
		close(fd);
		if (n == 16) return 0;
	}

	// 3. Fallback: generate and persist random 16-byte ID
	pii_random(buf, 16);
	fd = open(".system_id", O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR);
	if (fd >= 0) {
		write(fd, buf, 16);
		close(fd);
	}
	return 0;
}

extern char **environ;
extern char **host_argv;
static char pii_spawn_buf[4096];

//the host to start is this one, or its sibling, main_gui or main_tui,
//if the args start with -gui or -tui

static char pii_spawn_host[4096];

static const char *pii_spawn_which(const char **args)
{
	const char *want = NULL;
	if (!strncmp(*args, "-gui ", 5)) want = "main_gui";
	else if (!strncmp(*args, "-tui ", 5)) want = "main_tui";
	if (!want) return host_argv[0];
	*args += 5;
	const char *slash = strrchr(host_argv[0], '/');
	int dir = slash ? (int)(slash - host_argv[0] + 1) : 0;
	int n = snprintf(pii_spawn_host, sizeof(pii_spawn_host), "%.*s%s", dir, host_argv[0], want);
	if (n < 0 || n >= (int)sizeof(pii_spawn_host)) return host_argv[0];
	return pii_spawn_host;
}

int64_t pii_spawn(const char *args)
{
	//start another node, this host or its sibling, and this boot image,
	//with these args. returns its process id, or -1.
	char *argv[68];
	int argc = 0;
	if (!host_argv) return -1;
	const char *host = pii_spawn_which(&args);
	size_t len = strlen(args);
	if (!host_argv || len >= sizeof(pii_spawn_buf)) return -1;
	memcpy(pii_spawn_buf, args, len + 1);
	argv[argc++] = (char*)host;
	argv[argc++] = host_argv[1];
	for (char *tok = strtok(pii_spawn_buf, " "); tok && argc < 64; tok = strtok(NULL, " ")) argv[argc++] = tok;
	if (run_emu) argv[argc++] = (char*)"-e";
	argv[argc] = NULL;
	//no zombies when it exits, and it does not share the terminal input
	signal(SIGCHLD, SIG_IGN);
	posix_spawn_file_actions_t fa;
	posix_spawn_file_actions_init(&fa);
	posix_spawn_file_actions_addopen(&fa, 0, "/dev/null", O_RDONLY, 0);
	pid_t pid;
	int err = posix_spawn(&pid, argv[0], &fa, NULL, argv, environ);
	posix_spawn_file_actions_destroy(&fa);
	return err ? -1 : (int64_t)pid;
}

int64_t pii_pid()
{
	return (int64_t)getpid();
}

int64_t pii_alive(int64_t pid)
{
	//is this process running ? 1 if so, else 0
	if (pid <= 0) return 0;
	return (kill((pid_t)pid, 0) == 0 || errno == EPERM) ? 1 : 0;
}

int64_t pii_cpus()
{
	//the number of processors this machine has online
	long n = sysconf(_SC_NPROCESSORS_ONLN);
	return n < 1 ? 1 : (int64_t)n;
}

int64_t pii_memory()
{
	//the physical memory this machine has, in bytes
	long pages = sysconf(_SC_PHYS_PAGES);
	long size = sysconf(_SC_PAGESIZE);
	return (pages < 0 || size < 0) ? 0 : (int64_t)pages * (int64_t)size;
}

int64_t pii_host(char *buf, size_t len)
{
	//what this host program is, "cpu abi os", as the build names them
	const char *s = PII_HOST_CPU " " PII_HOST_ABI " " PII_HOST_OS;
	size_t n = strlen(s);
	if (len == 0) return 0;
	if (n >= len) n = len - 1;
	memcpy(buf, s, n);
	buf[n] = 0;
	return (int64_t)n;
}

int64_t pii_chmod(const char *path, uint64_t mode)
{
	//the mode of a file, who may read, write and run it
	return chmod(path, (mode_t)(mode & 07777)) == 0 ? 0 : -1;
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
	(void*)pii_sysid,
	(void*)pii_spawn,
	(void*)pii_pid,
	(void*)pii_alive,
	(void*)pii_cpus,
	(void*)pii_memory,
	(void*)pii_shm_open,
	(void*)pii_shm_close,
	(void*)pii_host,
	(void*)pii_chmod,
};

#endif