%ifndef SYSCALL_1234
%define SYSCALL_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;;;;
; syscall static class
;;;;;;;;;;;;;;;;;;;;;;

	def_class io
	def_method char, sys/write_char
	def_method string, sys/write_string
	def_method number, sys/write_number
	def_method read, sys/read_line

	def_class cpu
	def_method id, sys/cpu_get_id
	def_method time, sys/cpu_get_time
	def_method total, sys/cpu_get_total
	def_method opts, sys/opt_process

%ifidn OS, Darwin

;;;;;;;;;;;;;;;;;
; syscall for OSX
;;;;;;;;;;;;;;;;;

	sys_exit			equ	0x1
	sys_read			equ	0x3
	sys_write			equ	0x4
	sys_open			equ	0x5
	sys_close			equ	0x6
	sys_unlink			equ	0xa
	sys_ftruncate		equ	0xc9
	sys_stat			equ	0xbc
	sys_mmap			equ	0xc5
	sys_munmap			equ	0x49
	sys_mprotect		equ	0x4a
	sys_gettimeofday	equ	0x74

	prot_none			equ	0x0
	prot_read			equ	0x1
	prot_write			equ	0x2
	prot_exec			equ	0x4

	map_shared			equ	0x1
	map_private			equ	0x2
	map_fixed			equ	0x10
	map_anon			equ	0x1000

	o_rdonly			equ	0x0
	o_wronly			equ	0x1
	o_rdwr				equ	0x2
	o_trunc				equ	0x400
	o_append			equ	0x8
	o_nonblock			equ	0x4
	o_creat				equ	0x200
	o_excl				equ	0x800
	o_nofollow			equ	0x100
	o_cloexec			equ	0x1000000

	s_irwxu				equ	0x1c0
	s_irusr				equ	0x100
	s_iwusr				equ	0x80
	s_ixusr				equ	0x40
	s_irwxg				equ	0x38
	s_irgrp				equ	0x20
	s_iwgrp				equ	0x10
	s_ixgrp				equ	0x8
	s_irwxo				equ	0x7
	s_iroth				equ	0x4
	s_iwoth				equ	0x2
	s_ixoth				equ	0x1
	s_isuid				equ	0x800
	s_isgid				equ	0x400
	s_isvtx				equ	0x200

	stat_fsize			equ 0x48
	%assign stat_size	0x100

	def_structure	timeval
		def_long	timeval_sec
		def_long	timeval_usec
	def_structure_end

	def_structure	timezone
		def_int	timeval_minuteswest
		def_int	timeval_dsttime
	def_structure_end

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
		vp_cpy sys_exit + 0x2000000, r0
		vp_syscall
	%endmacro

	%macro sys_read 3
		;fd, buffer, length
		push_trashed
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy sys_read + 0x2000000, r0
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
		vp_cpy sys_read + 0x2000000, r0
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
		vp_cpy sys_write + 0x2000000, r0
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
		vp_cpy sys_write + 0x2000000, r0
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
		vp_cpy sys_mmap + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_mprotect 3
		;addr, len, prot
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size
		vp_cpy %3, r2		;read only etc
		vp_cpy sys_mprotect + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_munmap 2
		;addr, len
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy sys_munmap + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_stat 2
		;path, buffer
		push_trashed
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;stat buffer
		vp_cpy sys_stat + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_open 3
		;path, flags, mode
		push_trashed
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;flags
		vp_cpy %3, r2		;mode
		vp_cpy sys_open + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_close 1
		;fd
		push_trashed
		vp_cpy %1, r7		;fd
		vp_cpy sys_close + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_ftruncate 2
		;fd, offset
		push_trashed
		vp_cpy %1, r7		;fd
		vp_cpy %2, r6		;offset
		vp_cpy sys_ftruncate + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_unlink 1
		;name
		push_trashed
		vp_cpy %1, r7		;name
		vp_cpy sys_unlink + 0x2000000, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_gettimeofday 2
		;time, timezone
		push_trashed
		vp_push %1
		vp_cpy %1, r7		;time struct
		vp_cpy %2, r6		;timezone struct
		vp_cpy sys_gettimeofday + 0x2000000, r0
		vp_syscall
		vp_pop r1
		vp_cpy r0, [r1 + timeval_sec]
		vp_cpy r2, [r1 + timeval_usec]
		pop_trashed
	%endmacro

%elifidn OS, Linux

;;;;;;;;;;;;;;;;;;;
; syscall for Linux
;;;;;;;;;;;;;;;;;;;

	sys_exit			equ	0x3c
	sys_read			equ	0x0
	sys_write			equ	0x1
	sys_open			equ	0x2
	sys_close			equ	0x3
	sys_unlink			equ	0x57
	sys_ftruncate		equ	0x4d
	sys_stat			equ	0x4
	sys_mmap			equ	0x9
	sys_munmap			equ	0xb
	sys_mprotect		equ	0xa
	sys_gettimeofday	equ	0x60

	prot_none			equ	0x0
	prot_read			equ	0x1
	prot_write			equ	0x2
	prot_exec			equ	0x4

	map_shared			equ	0x1
	map_private			equ	0x2
	map_fixed			equ	0x10
	map_anon			equ	0x20

	o_rdonly			equ	0x0
	o_wronly			equ	0x1
	o_rdwr				equ	0x2
	o_trunc				equ	0x200
	o_append			equ	0x400
	o_nonblock			equ	0x800
	o_creat				equ	0x40
	o_excl				equ	0x80
	o_nofollow			equ	0x20000
	o_cloexec			equ	0x80000

	s_irwxu				equ	0x1c0
	s_irusr				equ	0x100
	s_iwusr				equ	0x80
	s_ixusr				equ	0x40
	s_irwxg				equ	0x38
	s_irgrp				equ	0x20
	s_iwgrp				equ	0x10
	s_ixgrp				equ	0x8
	s_irwxo				equ	0x7
	s_iroth				equ	0x4
	s_iwoth				equ	0x2
	s_ixoth				equ	0x1
	s_isuid				equ	0x800
	s_isgid				equ	0x400
	s_isvtx				equ	0x200

	stat_fsize			equ	0x30
	%assign stat_size	0x100

	def_structure	timeval
		def_long	timeval_sec
		def_long	timeval_usec
	def_structure_end

	def_structure	timezone
		def_int	timeval_minuteswest
		def_int	timeval_dsttime
	def_structure_end

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
		vp_cpy sys_exit, r0
		vp_syscall
	%endmacro

	%macro sys_read 3
		;fd, buffer, length
		push_trashed
		vp_cpy %1, r7
		vp_cpy %2, r6
		vp_cpy %3, r2
		vp_cpy sys_read, r0
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
		vp_cpy sys_read, r0
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
		vp_cpy sys_write, r0
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
		vp_cpy sys_write, r0
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
		vp_cpy sys_mmap, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_mprotect 3
		;addr, len, prot
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size
		vp_cpy %3, r2		;read only etc
		vp_cpy sys_mprotect, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_munmap 2
		;addr, len
		push_trashed
		vp_cpy %1, r7		;addr
		vp_cpy %2, r6		;size of the file
		vp_cpy sys_munmap, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_stat 2
		;path, buffer
		push_trashed
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;stat buffer
		vp_cpy sys_stat, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_open 3
		;path, flags, mode
		push_trashed
		vp_cpy %1, r7		;path
		vp_cpy %2, r6		;flags
		vp_cpy %3, r2		;mode
		vp_cpy sys_open, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_close 1
		;fd
		push_trashed
		vp_cpy %1, r7		;fd
		vp_cpy sys_close, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_ftruncate 2
		;fd, offset
		push_trashed
		vp_cpy %1, r7		;fd
		vp_cpy %2, r6		;offset
		vp_cpy sys_ftruncate, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_unlink 1
		;name
		push_trashed
		vp_cpy %1, r7		;name
		vp_cpy sys_unlink, r0
		vp_syscall
		pop_trashed
	%endmacro

	%macro sys_gettimeofday 2
		;time, timezone
		push_trashed
		vp_cpy %1, r7		;time struct
		vp_cpy %2, r6		;timezone struct
		vp_cpy sys_gettimeofday, r0
		vp_syscall
		pop_trashed
	%endmacro
%endif

%endif
