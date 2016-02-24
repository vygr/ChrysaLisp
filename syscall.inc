%ifndef SYSCALL_1234
	%define SYSCALL_1234

%include "vp.inc"

%ifidn OS, Darwin

;;;;;;;;;;;;;;;;;
; syscall for OSX
;;;;;;;;;;;;;;;;;

	SYS_EXIT			equ 0x2000001	;void exit(int rval)
	SYS_READ			equ 0x2000003	;user_ssize_t read(int fd, user_addr_t cbuf, user_size_t nbyte)
	SYS_WRITE			equ 0x2000004	;user_ssize_t write(int fd, user_addr_t cbuf, user_size_t nbyte)
	SYS_OPEN			equ 0x2000005	;int open(user_addr_t path, int flags, int mode)
	SYS_CLOSE			equ 0x2000006	;int close(int fd)
	SYS_MUNMAP			equ 0x2000049	;int munmap(caddr_t addr, size_t len)
	SYS_MPROTECT		equ 0x200004a	;int mprotect(caddr_t addr, size_t len, int prot)
	SYS_GETTIMEOFDAY	equ 0x2000074	;int gettimeofday(struct timeval *tp, struct timezone *tzp)
	SYS_MMAP			equ 0x20000c5	;user_addr_t mmap(caddr_t addr, size_t len, int prot, int flags, int fd, off_t pos)
	SYS_STAT			equ 0x20000bc	;int stat(user_addr_t path, user_addr_t ub)
	SYS_FTRUNCATE		equ 0x20000c9	;int ftruncate(int fd, off_t length)
	SYS_SHMOPEN			equ 0x200010a	;int shm_open(const char *name, int oflag, int mode)
	SYS_SHMUNLINK		equ 0x200010b	;int shm_unlink(const char *name)

	PROT_NONE	equ 0x0
	PROT_READ	equ 0x1
	PROT_WRITE	equ 0x2
	PROT_EXEC	equ 0x4

	MAP_SHARED	equ 0x1
	MAP_PRIVATE	equ 0x2
	MAP_FIXED	equ 0x10
	MAP_ANON	equ 0x1000

	O_RDONLY	equ 0x0000000			;open for reading only
	O_WRONLY	equ 0x0000001			;open for writing only
	O_RDWR		equ	0x0000002			;open for reading and writing
	O_NONBLOCK	equ 0x0000003			;do not block on open or for data to become available
	O_APPEND	equ 0x0000008			;append on each write
	O_CREAT		equ 0x0000200			;create file if it does not exist
	O_TRUNC		equ 0x0000400			;truncate size to 0
	O_EXCL		equ 0x0000800			;error if O_CREAT and the file exists
	O_SHLOCK	equ 0x0000010			;atomically obtain a shared lock
	O_EXLOCK	equ 0x0000020			;atomically obtain an exclusive lock
	O_NOFOLLOW	equ 0x0000100			;do not follow symlinks
	O_SYMLINK	equ 0x0200000			;allow open of symlinks
	O_EVTONLY	equ 0x0008000			;descriptor requested for event notifications only
	O_CLOEXEC	equ 0x1000000			;mark as close-on-exec

	S_IRWXU		equ 0x00001c0		;RWX mask for owner
	S_IRUSR		equ 0x0000100		;R for owner
	S_IWUSR		equ 0x0000080		;W for owner
	S_IXUSR		equ 0x0000040		;X for owner
	S_IRWXG		equ 0x0000038		;RWX mask for group
	S_IRGRP		equ 0x0000020		;R for group
	S_IWGRP		equ 0x0000010		;W for group
	S_IXGRP		equ 0x0000008		;X for group
	S_IRWXO		equ 0x0000007		;RWX mask for other
	S_IROTH		equ 0x0000003		;R for other
	S_IWOTH		equ 0x0000002		;W for other
	S_IXOTH		equ 0x0000001		;X for other
	S_ISUID		equ 0x0000800		;set user id on execution
	S_ISGID		equ 0x0000400		;set group id on execution
	S_ISVTX		equ 0x0000200		;save swapped text even after use

	struc STAT
		STAT_DEV:				resq 1	;ID of device containing file
		STAT_MODE:				resq 1	;Mode of file
		STAT_NLINK:				resq 1	;Number of hard links
		STAT_INO:				resd 1	;File serial number
		STAT_UID:				resd 1	;User ID of the file
		STAT_GID:				resd 1	;Group ID of the file
		STAT_RDEV:				resd 1	;Device ID
		STAT_ATIMESPEC:			resq 1	;time of last access
		STAT_MTIMESPEC:			resq 1	;time of last data modification
		STAT_CTIMESPEC:			resq 1	;time of last status change
		STAT_BIRTHTIMESPEC:		resq 1	;time of file creation(birth)
		STAT_FSIZE:				resq 1	;file size, in bytes
		STAT_BLOCKS:			resq 1	;blocks allocated for file
		STAT_BLKSIZE:			resq 1	;optimal blocksize for I/O
		STAT_FLAGS:				resq 1	;user defined flags for file
		STAT_GEN:				resq 1	;file generation number
		STAT_LSPARE:			resq 1	;reserved
		STAT_QSPARE:			resq 1	;reserved
		STAT_SIZE:
	endstruc

	struc TIMEVAL
		TIMEVAL_SEC:			resq 1
		TIMEVAL_USEC:			resq 1
		TIMEVAL_SIZE:
	endstruc

	struc TIMEZONE
		TIMEVAL_MINUTESWEST:	resd 1
		TIMEVAL_DSTTIME:		resd 1
		TIMEZONE_SIZE:
	endstruc

;;;;;;;;;;;;;;;;
; syscall macros
;;;;;;;;;;;;;;;;

	%macro push_trashed_1 0
		;pushes onto r4 (rsp)
		vp_push r1
		vp_push r2
		vp_push r6
		vp_push r7
		vp_push r8
		vp_push r9
		vp_push r10
		vp_push r11
	%endmacro

	%macro pop_trashed_1 0
		;pops from r4 (rsp)
		vp_pop r11
		vp_pop r10
		vp_pop r9
		vp_pop r8
		vp_pop r7
		vp_pop r6
		vp_pop r2
		vp_pop r1
	%endmacro

	%macro push_trashed_2 0
		;pushes onto r4 (rsp)
		vp_push r1
		vp_push r6
		vp_push r7
		vp_push r8
		vp_push r9
		vp_push r10
		vp_push r11
	%endmacro

	%macro pop_trashed_2 0
		;pops from r4 (rsp)
		vp_pop r11
		vp_pop r10
		vp_pop r9
		vp_pop r8
		vp_pop r7
		vp_pop r6
		vp_pop r1
	%endmacro

	%macro sys_exit 1
		;return code
		vp_cpy %1, r7
		vp_cpy SYS_EXIT, r0
		vp_syscall
	%endmacro

	%macro sys_read 3
		;fd, buffer, length
		push_trashed_1
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy SYS_READ, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_read_char 1
		;fd
		push_trashed_1
		vp_push 0
		vp_cpy %1, r7
		vp_cpy r4, r6
		vp_cpy 1, r2
		vp_cpy SYS_READ, r0
		vp_syscall
		vp_pop r0
		pop_trashed_1
	%endmacro

	%macro sys_write_string 3
		;fd, string, length
		push_trashed_1
		vp_push r0
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy SYS_WRITE, r0
		vp_syscall
		vp_pop r0
		pop_trashed_1
	%endmacro

	%macro sys_write_char 2
		;fd, char
		push_trashed_1
		vp_push r0
		vp_push %2
		vp_cpy %1, r7
		vp_cpy r4, r6
		vp_cpy 1, r2
		vp_cpy SYS_WRITE, r0
		vp_syscall
		vp_add 8, r4
		vp_pop r0
		pop_trashed_1
	%endmacro

	%macro sys_mmap 6
		;addr, len, prot, flags, fd, pos
		push_trashed_1
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy %3, r2		;read only etc
		vp_cpy %4, r10		;shared etc
		vp_cpy %5, r8		;file descriptor
		vp_cpy %6, r9		;offset
		vp_cpy SYS_MMAP, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_mprotect 3
		;addr, len, prot
		push_trashed_1
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size
		vp_cpy %3, r2		;read only etc
		vp_cpy SYS_MPROTECT, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_munmap 2
		;addr, len
		push_trashed_1
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy SYS_MUNMAP, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_stat 2
		;path, buffer
		push_trashed_1
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;stat buffer
		vp_cpy SYS_STAT, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_open 3
		;path, flags, mode
		push_trashed_1
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;flags
		vp_cpy %3, r2		;mode
		vp_cpy SYS_OPEN, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_close 1
		;fd
		push_trashed_1
		vp_cpy %1, r7		;fd
		vp_cpy SYS_CLOSE, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_ftruncate 2
		;fd, offset
		push_trashed_1
		vp_cpy %1, r7		;fd
		vp_cpy %2, r6		;offset
		vp_cpy SYS_FTRUNCATE, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_shmopen 3
		;name, flag, mode
		push_trashed_1
		vp_cpy %1, r7		;name
		vp_cpy %2, r6		;flag
		vp_cpy %3, r2		;mode
		vp_cpy SYS_SHMOPEN, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_shmunlink 1
		;name
		push_trashed_1
		vp_cpy %1, r7		;name
		vp_cpy SYS_SHMUNLINK, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_gettimeofday 2
		;time, timezone
		push_trashed_2
		vp_cpy %1, r7		;time struct
		vp_cpy %2, r6		;timezone struct
		vp_cpy SYS_GETTIMEOFDAY, r0
		vp_syscall
		pop_trashed_2
	%endmacro

%elifidn OS, Linux

;;;;;;;;;;;;;;;;;;;
; syscall for Linux
;;;;;;;;;;;;;;;;;;;

	SYS_EXIT			equ	0x3c
	SYS_READ			equ	0x0
	SYS_WRITE			equ	0x1
	SYS_OPEN			equ	0x2
	SYS_CLOSE			equ	0x3
	SYS_FTRUNCATE		equ	0x4d
	SYS_STAT			equ	0x4
	SYS_MMAP			equ	0x9
	SYS_MUNMAP			equ	0xb
	SYS_MPROTECT		equ	0xa
	SYS_GETTIMEOFDAY	equ	0x60
	SYS_SHMOPEN			equ 0x200010a	;int shm_open(const char *name, int oflag, int mode)
	SYS_SHMUNLINK		equ 0x200010b	;int shm_unlink(const char *name)

	PROT_NONE			equ	0x0
	PROT_READ			equ	0x1
	PROT_WRITE			equ	0x2
	PROT_EXEC			equ	0x4

	MAP_SHARED			equ	0x1
	MAP_PRIVATE			equ	0x2
	MAP_FIXED			equ	0x10
	MAP_ANON			equ	0x20

	O_RDONLY			equ	0x0
	O_WRONLY			equ	0x1
	O_RDWR				equ	0x2
	O_TRUNC				equ	0x200
	O_APPEND			equ	0x400
	O_NONBLOCK			equ	0x800
	O_CREAT				equ	0x40
	O_EXCL				equ	0x80
	O_NOFOLLOW			equ	0x20000
	O_CLOEXEC			equ	0x80000

	S_IRWXU				equ	0x1c0
	S_IRUSR				equ	0x100
	S_IWUSR				equ	0x80
	S_IXUSR				equ	0x40
	S_IRWXG				equ	0x38
	S_IRGRP				equ	0x20
	S_IWGRP				equ	0x10
	S_IXGRP				equ	0x8
	S_IRWXO				equ	0x7
	S_IROTH				equ	0x4
	S_IWOTH				equ	0x2
	S_IXOTH				equ	0x1
	S_ISUID				equ	0x800
	S_ISGID				equ	0x400
	S_ISVTX				equ	0x200

	struc STAT
		STAT_DEV:				resq 1	;ID of device containing file
		STAT_INO:				resq 1	;File serial number
		STAT_MODE:				resd 1	;Mode of file
		STAT_NLINK:				resd 1	;Number of hard links
		STAT_UID:				resd 1	;User ID of the file
		STAT_GID:				resd 1	;Group ID of the file
		STAT_RDEV:				resq 1	;Device ID
		STAT_PAD1:				resq 1
		STAT_FSIZE:				resq 1	;file size, in bytes
		STAT_BLKSIZE:			resd 1	;optimal blocksize for I/O
		STAT_PAD2:				resd 1
		STAT_BLOCKS:			resq 1	;blocks allocated for file
		STAT_ATIMESPEC:			resq 1	;time of last access
		STAT_ATIMESPEC_NANO:	resq 1	;time of last access
		STAT_MTIMESPEC:			resq 1	;time of last data modification
		STAT_MTIMESPEC_NANO:	resq 1	;time of last data modification
		STAT_CTIMESPEC:			resq 1	;time of last status change
		STAT_CTIMESPEC_NANO:	resq 1	;time of last status change
		STAT_UNUSED4:			resd 1
		STAT_UNUSED5:			resd 1
		STAT_PADDING:			resq 16
		STAT_SIZE:
	endstruc

	struc TIMEVAL
		TIMEVAL_SEC:			resq 1
		TIMEVAL_USEC:			resq 1
		TIMEVAL_SIZE:
	endstruc

	struc TIMEZONE
		TIMEVAL_MINUTESWEST:	resd 1
		TIMEVAL_DSTTIME:		resd 1
		TIMEZONE_SIZE:
	endstruc

;;;;;;;;;;;;;;;;
; syscall macros
;;;;;;;;;;;;;;;;

	%macro push_trashed_1 0
		;pushes onto r4 (rsp)
		vp_push r1
		vp_push r2
		vp_push r6
		vp_push r7
		vp_push r8
		vp_push r9
		vp_push r10
		vp_push r11
	%endmacro

	%macro pop_trashed_1 0
		;pops from r4 (rsp)
		vp_pop r11
		vp_pop r10
		vp_pop r9
		vp_pop r8
		vp_pop r7
		vp_pop r6
		vp_pop r2
		vp_pop r1
	%endmacro

	%macro push_trashed_2 0
		;pushes onto r4 (rsp)
		vp_push r1
		vp_push r6
		vp_push r7
		vp_push r8
		vp_push r9
		vp_push r10
		vp_push r11
	%endmacro

	%macro pop_trashed_2 0
		;pops from r4 (rsp)
		vp_pop r11
		vp_pop r10
		vp_pop r9
		vp_pop r8
		vp_pop r7
		vp_pop r6
		vp_pop r1
	%endmacro

	%macro sys_exit 1
		;return code
		vp_cpy %1, r7
		vp_cpy SYS_EXIT, r0
		vp_syscall
	%endmacro

	%macro sys_read 3
		;fd, buffer, length
		push_trashed_1
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy SYS_READ, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_read_char 1
		;fd
		push_trashed_1
		vp_push 0
		vp_cpy %1, r7
		vp_cpy r4, r6
		vp_cpy 1, r2
		vp_cpy SYS_READ, r0
		vp_syscall
		vp_pop r0
		pop_trashed_1
	%endmacro

	%macro sys_write_string 3
		;fd, string, length
		push_trashed_1
		vp_push r0
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy SYS_WRITE, r0
		vp_syscall
		vp_pop r0
		pop_trashed_1
	%endmacro

	%macro sys_write_char 2
		;fd, char
		push_trashed_1
		vp_push r0
		vp_push %2
		vp_cpy %1, r7
		vp_cpy r4, r6
		vp_cpy 1, r2
		vp_cpy SYS_WRITE, r0
		vp_syscall
		vp_add 8, r4
		vp_pop r0
		pop_trashed_1
	%endmacro

	%macro sys_mmap 6
		;addr, len, prot, flags, fd, pos
		push_trashed_1
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy %3, r2		;read only etc
		vp_cpy %4, r10		;shared etc
		vp_cpy %5, r8		;file descriptor
		vp_cpy %6, r9		;offset
		vp_cpy SYS_MMAP, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_mprotect 3
		;addr, len, prot
		push_trashed_1
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size
		vp_cpy %3, r2		;read only etc
		vp_cpy SYS_MPROTECT, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_munmap 2
		;addr, len
		push_trashed_1
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy SYS_MUNMAP, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_stat 2
		;path, buffer
		push_trashed_1
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;stat buffer
		vp_cpy SYS_STAT, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_open 3
		;path, flags, mode
		push_trashed_1
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;flags
		vp_cpy %3, r2		;mode
		vp_cpy SYS_OPEN, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_close 1
		;fd
		push_trashed_1
		vp_cpy %1, r7		;fd
		vp_cpy SYS_CLOSE, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_ftruncate 2
		;fd, offset
		push_trashed_1
		vp_cpy %1, r7		;fd
		vp_cpy %2, r6		;offset
		vp_cpy SYS_FTRUNCATE, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_shmopen 3
		;name, flag, mode
		push_trashed_1
		vp_cpy %1, r7		;name
		vp_cpy %2, r6		;flag
		vp_cpy %3, r2		;mode
		vp_cpy SYS_SHMOPEN, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_shmunlink 1
		;name
		push_trashed_1
		vp_cpy %1, r7		;name
		vp_cpy SYS_SHMUNLINK, r0
		vp_syscall
		pop_trashed_1
	%endmacro

	%macro sys_gettimeofday 2
		;time, timezone
		push_trashed_2
		vp_cpy %1, r7		;time struct
		vp_cpy %2, r6		;timezone struct
		vp_cpy SYS_GETTIMEOFDAY, r0
		vp_syscall
		pop_trashed_2
	%endmacro
%endif

%endif
