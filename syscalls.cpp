#include <sys/mman.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include <sys/syscall.h>
#include <stddef.h>
#include <sys/stat.h>

int main() {
	printf("\tsys_exit\tequ\t0x%x\n", sys_exit);
	printf("\tsys_read\tequ\t0x%x\n", sys_read);
	printf("\tsys_write\tequ\t0x%x\n", sys_write);
	printf("\tsys_open\tequ\t0x%x\n", sys_open);
	printf("\tsys_close\tequ\t0x%x\n", sys_close);
	printf("\tsys_unlink\tequ\t0x%x\n", sys_unlink);
	printf("\tsys_ftruncate\tequ\t0x%x\n", sys_ftruncate);
	printf("\tsys_stat\tequ\t0x%x\n", sys_stat);
	printf("\tsys_mmap\tequ\t0x%x\n", sys_mmap);
	printf("\tsys_munmap\tequ\t0x%x\n", sys_munmap);
	printf("\tsys_mprotect\tequ\t0x%x\n", sys_mprotect);
	printf("\tsys_gettimeofday\tequ\t0x%x\n\n", sys_gettimeofday);

	printf("\tprot_none\tequ\t0x%x\n", PROT_NONE);
	printf("\tprot_read\tequ\t0x%x\n", PROT_READ);
	printf("\tprot_write\tequ\t0x%x\n", PROT_WRITE);
	printf("\tprot_exec\tequ\t0x%x\n\n", PROT_EXEC);

	printf("\tmap_shared\tequ\t0x%x\n", MAP_SHARED);
	printf("\tmap_private\tequ\t0x%x\n", MAP_PRIVATE);
	printf("\tmap_fixed\tequ\t0x%x\n", MAP_FIXED);
	printf("\tmap_anon\tequ\t0x%x\n\n", MAP_ANON);

	printf("\to_rdonly\tequ\t0x%x\n", O_RDONLY);
	printf("\to_wronly\tequ\t0x%x\n", O_WRONLY);
	printf("\to_rdwr\tequ\t0x%x\n", O_RDWR);
	printf("\to_trunc\tequ\t0x%x\n", O_TRUNC);
	printf("\to_append\tequ\t0x%x\n", O_APPEND);
	printf("\to_nonblock\tequ\t0x%x\n", O_NONBLOCK);
	printf("\to_creat\tequ\t0x%x\n", O_CREAT);
	printf("\to_excl\tequ\t0x%x\n", O_EXCL);
	printf("\to_nofollow\tequ\t0x%x\n", O_NOFOLLOW);
	printf("\to_cloexec\tequ\t0x%x\n\n", O_CLOEXEC);

	printf("\ts_irwxu\tequ\t0x%x\n", S_IRWXU);
	printf("\ts_irusr\tequ\t0x%x\n", S_IRUSR);
	printf("\ts_iwusr\tequ\t0x%x\n", S_IWUSR);
	printf("\ts_ixusr\tequ\t0x%x\n", S_IXUSR);
	printf("\ts_irwxg\tequ\t0x%x\n", S_IRWXG);
	printf("\ts_irgrp\tequ\t0x%x\n", S_IRGRP);
	printf("\ts_iwgrp\tequ\t0x%x\n", S_IWGRP);
	printf("\ts_ixgrp\tequ\t0x%x\n", S_IXGRP);
	printf("\ts_irwxo\tequ\t0x%x\n", S_IRWXO);
	printf("\ts_iroth\tequ\t0x%x\n", S_IROTH);
	printf("\ts_iwoth\tequ\t0x%x\n", S_IWOTH);
	printf("\ts_ixoth\tequ\t0x%x\n", S_IXOTH);
	printf("\ts_isuid\tequ\t0x%x\n", S_ISUID);
	printf("\ts_isgid\tequ\t0x%x\n", S_ISGID);
	printf("\ts_isvtx\tequ\t0x%x\n\n", S_ISVTX);

	printf("\ts_ifmt\tequ\t0x%x\n", S_IFMT);
	printf("\ts_ifdir\tequ\t0x%x\n", S_IFDIR);
	printf("\ts_ifreg\tequ\t0x%x\n\n", S_IFREG);

	printf("\tstat_dev\tequ\t0x%lx\n", offsetof(struct stat, st_dev));
	printf("\tstat_mode\tequ\t0x%lx\n", offsetof(struct stat, st_mode));
	printf("\tstat_nlink\tequ\t0x%lx\n", offsetof(struct stat, st_nlink));
	printf("\tstat_ino\tequ\t0x%lx\n", offsetof(struct stat, st_ino));
	printf("\tstat_uid\tequ\t0x%lx\n", offsetof(struct stat, st_uid));
	printf("\tstat_gid\tequ\t0x%lx\n", offsetof(struct stat, st_gid));
	printf("\tstat_rdev\tequ\t0x%lx\n", offsetof(struct stat, st_rdev));
	printf("\tstat_atime\tequ\t0x%lx\n", offsetof(struct stat, st_atime));
	printf("\tstat_mtime\tequ\t0x%lx\n", offsetof(struct stat, st_mtime));
	printf("\tstat_ctime\tequ\t0x%lx\n", offsetof(struct stat, st_ctime));
	printf("\tstat_btime\tequ\t0x%lx\n", offsetof(struct stat, st_birthtime));
	printf("\tstat_fsize\tequ\t0x%lx\n", offsetof(struct stat, st_size));
	printf("\tstat_blocks\tequ\t0x%lx\n", offsetof(struct stat, st_blocks));
	printf("\tstat_blksize\tequ\t0x%lx\n", offsetof(struct stat, st_blksize));
	printf("\tstat_flags\tequ\t0x%lx\n", offsetof(struct stat, st_flags));
	printf("\tstat_gen\tequ\t0x%lx\n", offsetof(struct stat, st_gen));
	printf("\tstat_size\tequ\t0x%lx\n\n", sizeof(struct stat));

	return 0;
}
