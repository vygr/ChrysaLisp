#include <sys/mman.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include <sys/syscall.h>
#include <stddef.h>
#include <sys/stat.h>

int main() {
	printf("\tSYS_EXIT\tequ\t0x%x\n", SYS_exit);
	printf("\tSYS_READ\tequ\t0x%x\n", SYS_read);
	printf("\tSYS_WRITE\tequ\t0x%x\n", SYS_write);
	printf("\tSYS_OPEN\tequ\t0x%x\n", SYS_open);
	printf("\tSYS_CLOSE\tequ\t0x%x\n", SYS_close);
	printf("\tSYS_UNLINK\tequ\t0x%x\n", SYS_unlink);
	printf("\tSYS_FTRUNCATE\tequ\t0x%x\n", SYS_ftruncate);
	printf("\tSYS_STAT\tequ\t0x%x\n", SYS_stat);
	printf("\tSYS_MMAP\tequ\t0x%x\n", SYS_mmap);
	printf("\tSYS_MUNMAP\tequ\t0x%x\n", SYS_munmap);
	printf("\tSYS_MPROTECT\tequ\t0x%x\n", SYS_mprotect);
	printf("\tSYS_GETTIMEOFDAY\tequ\t0x%x\n\n", SYS_gettimeofday);

	printf("\tPROT_NONE\tequ\t0x%x\n", prot_none);
	printf("\tPROT_READ\tequ\t0x%x\n", prot_read);
	printf("\tPROT_WRITE\tequ\t0x%x\n", prot_write);
	printf("\tPROT_EXEC\tequ\t0x%x\n\n", prot_exec);

	printf("\tMAP_SHARED\tequ\t0x%x\n", map_shared);
	printf("\tMAP_PRIVATE\tequ\t0x%x\n", map_private);
	printf("\tMAP_FIXED\tequ\t0x%x\n", map_fixed);
	printf("\tMAP_ANON\tequ\t0x%x\n\n", map_anon);

	printf("\tO_RDONLY\tequ\t0x%x\n", O_RDONLY);
	printf("\tO_WRONLY\tequ\t0x%x\n", O_WRONLY);
	printf("\tO_RDWR\tequ\t0x%x\n", O_RDWR);
	printf("\tO_TRUNC\tequ\t0x%x\n", O_TRUNC);
	printf("\tO_APPEND\tequ\t0x%x\n", O_APPEND);
	printf("\tO_NONBLOCK\tequ\t0x%x\n", O_NONBLOCK);
	printf("\tO_CREAT\tequ\t0x%x\n", O_CREAT);
	printf("\tO_EXCL\tequ\t0x%x\n", O_EXCL);
	printf("\tO_NOFOLLOW\tequ\t0x%x\n", O_NOFOLLOW);
	printf("\tO_CLOEXEC\tequ\t0x%x\n\n", O_CLOEXEC);

	printf("\tS_IRWXU\tequ\t0x%x\n", S_IRWXU);
	printf("\tS_IRUSR\tequ\t0x%x\n", S_IRUSR);
	printf("\tS_IWUSR\tequ\t0x%x\n", S_IWUSR);
	printf("\tS_IXUSR\tequ\t0x%x\n", S_IXUSR);
	printf("\tS_IRWXG\tequ\t0x%x\n", S_IRWXG);
	printf("\tS_IRGRP\tequ\t0x%x\n", S_IRGRP);
	printf("\tS_IWGRP\tequ\t0x%x\n", S_IWGRP);
	printf("\tS_IXGRP\tequ\t0x%x\n", S_IXGRP);
	printf("\tS_IRWXO\tequ\t0x%x\n", S_IRWXO);
	printf("\tS_IROTH\tequ\t0x%x\n", S_IROTH);
	printf("\tS_IWOTH\tequ\t0x%x\n", S_IWOTH);
	printf("\tS_IXOTH\tequ\t0x%x\n", S_IXOTH);
	printf("\tS_ISUID\tequ\t0x%x\n", S_ISUID);
	printf("\tS_ISGID\tequ\t0x%x\n", S_ISGID);
	printf("\tS_ISVTX\tequ\t0x%x\n\n", S_ISVTX);

	printf("\tSTAT_DEV\tequ\t0x%lx\n", offsetof(struct stat, st_dev));
	printf("\tSTAT_MODE\tequ\t0x%lx\n", offsetof(struct stat, st_mode));
	printf("\tSTAT_NLINK\tequ\t0x%lx\n", offsetof(struct stat, st_nlink));
	printf("\tSTAT_INO\tequ\t0x%lx\n", offsetof(struct stat, st_ino));
	printf("\tSTAT_UID\tequ\t0x%lx\n", offsetof(struct stat, st_uid));
	printf("\tSTAT_GID\tequ\t0x%lx\n", offsetof(struct stat, st_gid));
	printf("\tSTAT_RDEV\tequ\t0x%lx\n", offsetof(struct stat, st_rdev));
	printf("\tSTAT_ATIME\tequ\t0x%lx\n", offsetof(struct stat, st_atime));
	printf("\tSTAT_MTIME\tequ\t0x%lx\n", offsetof(struct stat, st_mtime));
	printf("\tSTAT_CTIME\tequ\t0x%lx\n", offsetof(struct stat, st_ctime));
	printf("\tSTAT_BTIME\tequ\t0x%lx\n", offsetof(struct stat, st_birthtime));
	printf("\tSTAT_FSIZE\tequ\t0x%lx\n", offsetof(struct stat, st_size));
	printf("\tSTAT_BLOCKS\tequ\t0x%lx\n", offsetof(struct stat, st_blocks));
	printf("\tSTAT_BLKSIZE\tequ\t0x%lx\n", offsetof(struct stat, st_blksize));
	printf("\tSTAT_FLAGS\tequ\t0x%lx\n", offsetof(struct stat, st_flags));
	printf("\tSTAT_GEN\tequ\t0x%lx\n", offsetof(struct stat, st_gen));
	printf("\tSTAT_SIZE\tequ\t0x%lx\n\n", sizeof(struct stat));

	return 0;
}
