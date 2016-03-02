%ifndef SYSCALL_1234
	%define SYSCALL_1234

%include "inc/vp.inc"

%ifidn OS, Darwin

;;;;;;;;;;;;;;;;;
; syscall for OSX
;;;;;;;;;;;;;;;;;

	SYS_EXIT			equ	0x1
	SYS_READ			equ	0x3
	SYS_WRITE			equ	0x4
	SYS_OPEN			equ	0x5
	SYS_CLOSE			equ	0x6
	SYS_UNLINK			equ	0xa
	SYS_FTRUNCATE		equ	0xc9
	SYS_STAT			equ	0xbc
	SYS_MMAP			equ	0xc5
	SYS_MUNMAP			equ	0x49
	SYS_MPROTECT		equ	0x4a
	SYS_GETTIMEOFDAY	equ	0x74

	PROT_NONE			equ	0x0
	PROT_READ			equ	0x1
	PROT_WRITE			equ	0x2
	PROT_EXEC			equ	0x4

	MAP_SHARED			equ	0x1
	MAP_PRIVATE			equ	0x2
	MAP_FIXED			equ	0x10
	MAP_ANON			equ	0x1000

	O_RDONLY			equ	0x0
	O_WRONLY			equ	0x1
	O_RDWR				equ	0x2
	O_TRUNC				equ	0x400
	O_APPEND			equ	0x8
	O_NONBLOCK			equ	0x4
	O_CREAT				equ	0x200
	O_EXCL				equ	0x800
	O_NOFOLLOW			equ	0x100
	O_CLOEXEC			equ	0x1000000

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

	STAT_FSIZE			equ 0x48
	STAT_SIZE			equ 0x100

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

	%macro push_trashed 0
		;pushes onto r4 (rsp)
		vp_push r1, r2, r6, r7, r8, r9, r10, r11
	%endmacro

	%macro pop_trashed 0
		;pops from r4 (rsp)
		vp_pop r1, r2, r6, r7, r8, r9, r10, r11
	%endmacro

	%macro sys_exit 1
		;return code
		vp_cpy %1, r7
		vp_cpy SYS_EXIT + 0x2000000, r0
		vp_syscall
	%endmacro

	%macro sys_read 3
		;fd, buffer, length
		push_trashed
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy SYS_READ + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_read_char 1
		;fd
		push_trashed
		vp_push 0
		vp_cpy %1, r7
		vp_cpy r4, r6
		vp_cpy 1, r2
		vp_cpy SYS_READ + 0x2000000, r0
		vp_syscall
		vp_pop r0
		pop_trashed
	%endmacro

	%macro sys_write_string 3
		;fd, string, length
		push_trashed
		vp_push r0
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy SYS_WRITE + 0x2000000, r0
		vp_syscall
		vp_pop r0
		pop_trashed
	%endmacro

	%macro sys_write_char 2
		;fd, char
		push_trashed
		vp_push r0, %2
		vp_cpy %1, r7
		vp_cpy r4, r6
		vp_cpy 1, r2
		vp_cpy SYS_WRITE + 0x2000000, r0
		vp_syscall
		vp_add 8, r4
		vp_pop r0
		pop_trashed
	%endmacro

	%macro sys_mmap 6
		;addr, len, prot, flags, fd, pos
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy %3, r2		;read only etc
		vp_cpy %4, r10		;shared etc
		vp_cpy %5, r8		;file descriptor
		vp_cpy %6, r9		;offset
		vp_cpy SYS_MMAP + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_mprotect 3
		;addr, len, prot
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size
		vp_cpy %3, r2		;read only etc
		vp_cpy SYS_MPROTECT + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_munmap 2
		;addr, len
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy SYS_MUNMAP + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_stat 2
		;path, buffer
		push_trashed
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;stat buffer
		vp_cpy SYS_STAT + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_open 3
		;path, flags, mode
		push_trashed
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;flags
		vp_cpy %3, r2		;mode
		vp_cpy SYS_OPEN + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_close 1
		;fd
		push_trashed
		vp_cpy %1, r7		;fd
		vp_cpy SYS_CLOSE + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_ftruncate 2
		;fd, offset
		push_trashed
		vp_cpy %1, r7		;fd
		vp_cpy %2, r6		;offset
		vp_cpy SYS_FTRUNCATE + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_unlink 1
		;name
		push_trashed
		vp_cpy %1, r7		;name
		vp_cpy SYS_UNLINK + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_gettimeofday 2
		;time, timezone
		push_trashed
		vp_push %1
		vp_cpy %1, r7		;time struct
		vp_cpy %2, r6		;timezone struct
		vp_cpy SYS_GETTIMEOFDAY + 0x2000000, r0
		vp_syscall
		vp_pop r1
		vp_cpy r0, [r1 + TIMEVAL_SEC]
		vp_cpy r2, [r1 + TIMEVAL_USEC]
		pop_trashed
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
	SYS_UNLINK			equ	0x57
	SYS_FTRUNCATE		equ	0x4d
	SYS_STAT			equ	0x4
	SYS_MMAP			equ	0x9
	SYS_MUNMAP			equ	0xb
	SYS_MPROTECT		equ	0xa
	SYS_GETTIMEOFDAY	equ	0x60

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

	STAT_FSIZE			equ	0x30
	STAT_SIZE			equ	0x100

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

	%macro push_trashed 0
		;pushes onto r4 (rsp)
		vp_push r1, r2, r6, r7, r8, r9, r10, r11
	%endmacro

	%macro pop_trashed 0
		;pops from r4 (rsp)
		vp_pop r1, r2, r6, r7, r8, r9, r10, r11
	%endmacro

	%macro sys_exit 1
		;return code
		vp_cpy %1, r7
		vp_cpy SYS_EXIT, r0
		vp_syscall
	%endmacro

	%macro sys_read 3
		;fd, buffer, length
		push_trashed
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy SYS_READ, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_read_char 1
		;fd
		push_trashed
		vp_push 0
		vp_cpy %1, r7
		vp_cpy r4, r6
		vp_cpy 1, r2
		vp_cpy SYS_READ, r0
		vp_syscall
		vp_pop r0
		pop_trashed
	%endmacro

	%macro sys_write_string 3
		;fd, string, length
		push_trashed
		vp_push r0
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy SYS_WRITE, r0
		vp_syscall
		vp_pop r0
		pop_trashed
	%endmacro

	%macro sys_write_char 2
		;fd, char
		push_trashed
		vp_push r0, %2
		vp_cpy %1, r7
		vp_cpy r4, r6
		vp_cpy 1, r2
		vp_cpy SYS_WRITE, r0
		vp_syscall
		vp_add 8, r4
		vp_pop r0
		pop_trashed
	%endmacro

	%macro sys_mmap 6
		;addr, len, prot, flags, fd, pos
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy %3, r2		;read only etc
		vp_cpy %4, r10		;shared etc
		vp_cpy %5, r8		;file descriptor
		vp_cpy %6, r9		;offset
		vp_cpy SYS_MMAP, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_mprotect 3
		;addr, len, prot
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size
		vp_cpy %3, r2		;read only etc
		vp_cpy SYS_MPROTECT, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_munmap 2
		;addr, len
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy SYS_MUNMAP, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_stat 2
		;path, buffer
		push_trashed
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;stat buffer
		vp_cpy SYS_STAT, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_open 3
		;path, flags, mode
		push_trashed
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;flags
		vp_cpy %3, r2		;mode
		vp_cpy SYS_OPEN, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_close 1
		;fd
		push_trashed
		vp_cpy %1, r7		;fd
		vp_cpy SYS_CLOSE, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_ftruncate 2
		;fd, offset
		push_trashed
		vp_cpy %1, r7		;fd
		vp_cpy %2, r6		;offset
		vp_cpy SYS_FTRUNCATE, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_unlink 1
		;name
		push_trashed
		vp_cpy %1, r7		;name
		vp_cpy SYS_UNLINK, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_gettimeofday 2
		;time, timezone
		push_trashed
		vp_cpy %1, r7		;time struct
		vp_cpy %2, r6		;timezone struct
		vp_cpy SYS_GETTIMEOFDAY, r0
		vp_syscall
		pop_trashed
	%endmacro
%endif

%endif
