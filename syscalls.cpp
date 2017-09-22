#include <sys/mman.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include <sys/syscall.h>
#include <stddef.h>
#include <sys/stat.h>
#include <sys/unistd.h>

int main() {
	printf("(defcvar 'sys_openat 0x%x)\n", SYS_openat);
	printf("(defcvar 'sys_unlinkat 0x%x)\n", SYS_unlinkat);
	printf("(defcvar 'sys_fstatat 0x%x)\n", SYS_fstatat);
//	printf("(defcvar 'sys_stat 0x%x)\n", SYS_stat);
	printf("(defcvar 'sys_exit 0x%x)\n", SYS_exit);
	printf("(defcvar 'sys_read 0x%x)\n", SYS_read);
	printf("(defcvar 'sys_write 0x%x)\n", SYS_write);
	printf("(defcvar 'sys_close 0x%x)\n", SYS_close);
	printf("(defcvar 'sys_ftruncate 0x%x)\n", SYS_ftruncate);
	printf("(defcvar 'sys_mmap 0x%x)\n", SYS_mmap);
	printf("(defcvar 'sys_munmap 0x%x)\n", SYS_munmap);
	printf("(defcvar 'sys_mprotect 0x%x)\n", SYS_mprotect);
	printf("(defcvar 'sys_gettimeofday 0x%x)\n", SYS_gettimeofday);
	printf("(defcvar 'sys_fcntl 0x%x)\n\n", SYS_fcntl);

	printf("(defcvar 'at_fdcwd 0x%x)\n\n", AT_FDCWD);
	
	printf("(defcvar 'prot_none 0x%x)\n", PROT_NONE);
	printf("(defcvar 'prot_read 0x%x)\n", PROT_READ);
	printf("(defcvar 'prot_write 0x%x)\n", PROT_WRITE);
	printf("(defcvar 'prot_exec 0x%x)\n\n", PROT_EXEC);

	printf("(defcvar 'map_shared 0x%x)\n", MAP_SHARED);
	printf("(defcvar 'map_private 0x%x)\n", MAP_PRIVATE);
	printf("(defcvar 'map_fixed 0x%x)\n", MAP_FIXED);
	printf("(defcvar 'map_anon 0x%x)\n\n", MAP_ANON);

	printf("(defcvar 'o_rdonly 0x%x)\n", O_RDONLY);
	printf("(defcvar 'o_wronly 0x%x)\n", O_WRONLY);
	printf("(defcvar 'o_rdwr 0x%x)\n", O_RDWR);
	printf("(defcvar 'o_trunc 0x%x)\n", O_TRUNC);
	printf("(defcvar 'o_append 0x%x)\n", O_APPEND);
	printf("(defcvar 'o_nonblock 0x%x)\n", O_NONBLOCK);
	printf("(defcvar 'o_creat 0x%x)\n", O_CREAT);
	printf("(defcvar 'o_excl 0x%x)\n", O_EXCL);
	printf("(defcvar 'o_nofollow 0x%x)\n", O_NOFOLLOW);
	printf("(defcvar 'o_cloexec 0x%x)\n\n", O_CLOEXEC);

	printf("(defcvar 's_irwxu 0x%x)\n", S_IRWXU);
	printf("(defcvar 's_irusr 0x%x)\n", S_IRUSR);
	printf("(defcvar 's_iwusr 0x%x)\n", S_IWUSR);
	printf("(defcvar 's_ixusr 0x%x)\n", S_IXUSR);
	printf("(defcvar 's_irwxg 0x%x)\n", S_IRWXG);
	printf("(defcvar 's_irgrp 0x%x)\n", S_IRGRP);
	printf("(defcvar 's_iwgrp 0x%x)\n", S_IWGRP);
	printf("(defcvar 's_ixgrp 0x%x)\n", S_IXGRP);
	printf("(defcvar 's_irwxo 0x%x)\n", S_IRWXO);
	printf("(defcvar 's_iroth 0x%x)\n", S_IROTH);
	printf("(defcvar 's_iwoth 0x%x)\n", S_IWOTH);
	printf("(defcvar 's_ixoth 0x%x)\n", S_IXOTH);
	printf("(defcvar 's_isuid 0x%x)\n", S_ISUID);
	printf("(defcvar 's_isgid 0x%x)\n", S_ISGID);
	printf("(defcvar 's_isvtx 0x%x)\n\n", S_ISVTX);

	printf("(defcvar 's_ifmt 0x%x)\n", S_IFMT);
	printf("(defcvar 's_ifdir 0x%x)\n", S_IFDIR);
	printf("(defcvar 's_ifreg 0x%x)\n\n", S_IFREG);

	printf("(defcvar 'f_getfl 0x%x)\n", F_GETFL);
	printf("(defcvar 'f_setfl 0x%x)\n\n", F_SETFL);

	printf("(defcvar 'stat_dev 0x%lx)\n", offsetof(struct stat, st_dev));
	printf("(defcvar 'stat_mode 0x%lx)\n", offsetof(struct stat, st_mode));
	printf("(defcvar 'stat_nlink 0x%lx)\n", offsetof(struct stat, st_nlink));
	printf("(defcvar 'stat_ino 0x%lx)\n", offsetof(struct stat, st_ino));
	printf("(defcvar 'stat_uid 0x%lx)\n", offsetof(struct stat, st_uid));
	printf("(defcvar 'stat_gid 0x%lx)\n", offsetof(struct stat, st_gid));
	printf("(defcvar 'stat_rdev 0x%lx)\n", offsetof(struct stat, st_rdev));
	printf("(defcvar 'stat_atime 0x%lx)\n", offsetof(struct stat, st_atime));
	printf("(defcvar 'stat_mtime 0x%lx)\n", offsetof(struct stat, st_mtime));
	printf("(defcvar 'stat_ctime 0x%lx)\n", offsetof(struct stat, st_ctime));
//	printf("(defcvar 'stat_btime 0x%lx)\n", offsetof(struct stat, st_birthtime));
	printf("(defcvar 'stat_fsize 0x%lx)\n", offsetof(struct stat, st_size));
	printf("(defcvar 'stat_blocks 0x%lx)\n", offsetof(struct stat, st_blocks));
	printf("(defcvar 'stat_blksize 0x%lx)\n", offsetof(struct stat, st_blksize));
//	printf("(defcvar 'stat_flags 0x%lx)\n", offsetof(struct stat, st_flags));
//	printf("(defcvar 'stat_gen 0x%lx)\n", offsetof(struct stat, st_gen));
	printf("(defcvar 'stat_size 0x%lx)\n\n", sizeof(struct stat));

	return 0;
}
