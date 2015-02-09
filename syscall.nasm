%ifndef SYSCALL_1234
    %define SYSCALL_1234

%include "vp.nasm"

;;;;;;;;;;;;;;;;;
; syscall numbers
;;;;;;;;;;;;;;;;;

	SYS_EXIT	equ 0x2000001	;void exit(int rval)
	SYS_READ	equ 0x2000003	;user_ssize_t read(int fd, user_addr_t cbuf, user_size_t nbyte)
	SYS_WRITE	equ 0x2000004	;user_ssize_t write(int fd, user_addr_t cbuf, user_size_t nbyte)
	SYS_OPEN	equ 0x2000005	;int open(user_addr_t path, int flags, int mode)
	SYS_CLOSE	equ 0x2000006	;int close(int fd)
	SYS_MUNMAP	equ 0x2000049	;int munmap(caddr_t addr, size_t len)
	SYS_MMAP	equ 0x20000c5	;user_addr_t mmap(caddr_t addr, size_t len, int prot, int flags, int fd, off_t pos)

	PROT_NONE	equ 0x0
	PROT_READ	equ 0x1
	PROT_WRITE	equ 0x2
	PROT_EXEC	equ 0x4

	MAP_SHARED	equ 0x1
	MAP_PRIVATE	equ 0x2
	MAP_FIXED	equ 0x10
	MAP_ANON	equ 0x1000

;;;;;;;;;;;;;;;;
; syscall macros
;;;;;;;;;;;;;;;;

	%macro push_trashed 0
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

	%macro pop_trashed 0
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

	%macro sys_exit 1
		;return code
		vp_cpy %1, r7
		vp_cpy SYS_EXIT, r0
		vp_syscall
	%endmacro

	%macro sys_read_string 3
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
		vp_push r0
		vp_push %2
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

	%macro sys_munmap 2
		;addr, len
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy SYS_MUNMAP, r0
		vp_syscall
		pop_trashed
	%endmacro

%endif
