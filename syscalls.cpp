#include <sys/mman.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include <sys/syscall.h>
#include <stddef.h>
#include <sys/stat.h>

int main() {
	printf("\tsys_exit\tequ\t0x%x\n", SYS_exit);
	printf("\tsys_read\tequ\t0x%x\n", SYS_read);
	printf("\tsys_write\tequ\t0x%x\n", SYS_write);
	printf("\tsys_open\tequ\t0x%x\n", SYS_open);
	printf("\tsys_close\tequ\t0x%x\n", SYS_close);
	printf("\tsys_unlink\tequ\t0x%x\n", SYS_unlink);
	printf("\tsys_ftruncate\tequ\t0x%x\n", SYS_ftruncate);
	printf("\tsys_stat\tequ\t0x%x\n", SYS_stat);
	printf("\tsys_mmap\tequ\t0x%x\n", SYS_mmap);
	printf("\tsys_munmap\tequ\t0x%x\n", SYS_munmap);
	printf("\tsys_mprotect\tequ\t0x%x\n", SYS_mprotect);
	printf("\tsys_gettimeofday\tequ\t0x%x\n\n", SYS_gettimeofday);

	printf("\tprot_none\tequ\t0x%x\n", prot_none);
	printf("\tprot_read\tequ\t0x%x\n", prot_read);
	printf("\tprot_write\tequ\t0x%x\n", prot_write);
	printf("\tprot_exec\tequ\t0x%x\n\n", prot_exec);

	printf("\tmap_shared\tequ\t0x%x\n", map_shared);
	printf("\tmap_private\tequ\t0x%x\n", map_private);
	printf("\tmap_fixed\tequ\t0x%x\n", map_fixed);
	printf("\tmap_anon\tequ\t0x%x\n\n", map_anon);

	printf("\to_rdonly\tequ\t0x%x\n", o_rdonly);
	printf("\to_wronly\tequ\t0x%x\n", o_wronly);
	printf("\to_rdwr\tequ\t0x%x\n", o_rdwr);
	printf("\to_trunc\tequ\t0x%x\n", o_trunc);
	printf("\to_append\tequ\t0x%x\n", o_append);
	printf("\to_nonblock\tequ\t0x%x\n", o_nonblock);
	printf("\to_creat\tequ\t0x%x\n", o_creat);
	printf("\to_excl\tequ\t0x%x\n", o_excl);
	printf("\to_nofollow\tequ\t0x%x\n", o_nofollow);
	printf("\to_cloexec\tequ\t0x%x\n\n", o_cloexec);

	printf("\ts_irwxu\tequ\t0x%x\n", s_irwxu);
	printf("\ts_irusr\tequ\t0x%x\n", s_irusr);
	printf("\ts_iwusr\tequ\t0x%x\n", s_iwusr);
	printf("\ts_ixusr\tequ\t0x%x\n", s_ixusr);
	printf("\ts_irwxg\tequ\t0x%x\n", s_irwxg);
	printf("\ts_irgrp\tequ\t0x%x\n", s_irgrp);
	printf("\ts_iwgrp\tequ\t0x%x\n", s_iwgrp);
	printf("\ts_ixgrp\tequ\t0x%x\n", s_ixgrp);
	printf("\ts_irwxo\tequ\t0x%x\n", s_irwxo);
	printf("\ts_iroth\tequ\t0x%x\n", s_iroth);
	printf("\ts_iwoth\tequ\t0x%x\n", s_iwoth);
	printf("\ts_ixoth\tequ\t0x%x\n", s_ixoth);
	printf("\ts_isuid\tequ\t0x%x\n", s_isuid);
	printf("\ts_isgid\tequ\t0x%x\n", s_isgid);
	printf("\ts_isvtx\tequ\t0x%x\n\n", s_isvtx);

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
