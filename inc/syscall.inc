(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;;;;;;
; syscall static class
;;;;;;;;;;;;;;;;;;;;;;

(def-class 'sys_io)
(def-method 'char 'sys/write_char 'static '(r0 r1))
(def-method 'string 'sys/write_string 'static '(r0 r1))
(def-method 'number 'sys/write_number 'static '(r0 r1 r2))
(def-method 'read 'sys/read_line 'static '(r0 r1 r2) '(r0))
(def-method 'debug_long 'sys/write_debug_long 'static '(r0 r1 r2 r3))
(def-method 'debug_str 'sys/write_debug_str 'static '(r0 r1 r2 r3))

(def-class 'sys_cpu)
(def-method 'id 'sys/cpu_get_id 'static '() '(r0))
(def-method 'time 'sys/cpu_get_time 'static '() '(r0))
(def-method 'total 'sys/cpu_get_total 'static '() '(r0))
(def-method 'opts 'sys/opt_process,static '{r0})

%macro sys_call 1
	vp_cpy %1, r0
	syscall
%endmacro

%ifidn OS, Darwin

;;;;;;;;;;;;;;;;;
; syscall for OSX
;;;;;;;;;;;;;;;;;

(equate 'sys_exit 0x1)
(equate 'sys_read 0x3)
(equate 'sys_write 0x4)
(equate 'sys_open 0x5)
(equate 'sys_close 0x6)
(equate 'sys_unlink 0xa)
(equate 'sys_ftruncate 0xc9)
(equate 'sys_stat 0xbc)
(equate 'sys_mmap 0xc5)
(equate 'sys_munmap 0x49)
(equate 'sys_mprotect 0x4a)
(equate 'sys_gettimeofday 0x74)

(equate 'prot_none 0x0)
(equate 'prot_read 0x1)
(equate 'prot_write 0x2)
(equate 'prot_exec 0x4)

(equate 'map_shared 0x1)
(equate 'map_private 0x2)
(equate 'map_fixed 0x10)
(equate 'map_anon 0x1000)

(equate 'o_rdonly 0x0)
(equate 'o_wronly 0x1)
(equate 'o_rdwr 0x2)
(equate 'o_trunc 0x400)
(equate 'o_append 0x8)
(equate 'o_nonblock 0x4)
(equate 'o_creat 0x200)
(equate 'o_excl 0x800)
(equate 'o_nofollow 0x100)
(equate 'o_cloexec 0x1000000)

(equate 's_irwxu 0x1c0)
(equate 's_irusr 0x100)
(equate 's_iwusr 0x80)
(equate 's_ixusr 0x40)
(equate 's_irwxg 0x38)
(equate 's_irgrp 0x20)
(equate 's_iwgrp 0x10)
(equate 's_ixgrp 0x8)
(equate 's_irwxo 0x7)
(equate 's_iroth 0x4)
(equate 's_iwoth 0x2)
(equate 's_ixoth 0x1)
(equate 's_isuid 0x800)
(equate 's_isgid 0x400)
(equate 's_isvtx 0x200)

(equate 's_ifmt 0xf000)
(equate 's_ifdir 0x4000)
(equate 's_ifreg 0x8000)

(equate 'stat_mode 0x8)
(equate 'stat_fsize 0x48)
(equate 'stat_size 0x100)

(def-struct 'timeval)
	(long 'timeval_sec)
	(long 'timeval_usec)
(def-struct-end)

(def-struct 'timezone)
	int	timeval_minuteswest
	int	timeval_dsttime
(def-struct-end)

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
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	sys_call sys_exit + 0x2000000
%endmacro

%macro sys_read 3
	;fd, buffer, length
	push_trashed
	(set-src %1, %2, %3)
	(set-dst r7, r6, r2)
	(map-src-to-dst)
	sys_call sys_read + 0x2000000
	pop_trashed
%endmacro

%macro sys_read_char 1
	;fd
	push_trashed
	vp_push 0
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	(vp-cpy-rr r4 r6)
	vp_cpy 1, r2
	sys_call sys_read + 0x2000000
	vp_pop r0
	pop_trashed
%endmacro

%macro sys_write_string 3
	;fd, string, length
	push_trashed
	vp_push r0
	(set-src %1, %2, %3)
	(set-dst r7, r6, r2)
	(map-src-to-dst)
	sys_call sys_write + 0x2000000
	vp_pop r0
	pop_trashed
%endmacro

%macro sys_write_char 2
	;fd, char
	push_trashed
	vp_push r0, %2
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	(vp-cpy-rr r4 r6)
	vp_cpy 1, r2
	sys_call sys_write + 0x2000000
	(vp-add-cr 8 r4)
	vp_pop r0
	pop_trashed
%endmacro

%macro sys_mmap 6
	;addr, len, prot, flags, fd, pos
	push_trashed
	(set-src %1, %2, %3, %4, %5, %6)
	(set-dst r7, r6, r2, r10, r8, r9)
	(map-src-to-dst)
	sys_call sys_mmap + 0x2000000
	pop_trashed
%endmacro

%macro sys_mprotect 3
	;addr, len, prot
	push_trashed
	(set-src %1, %2, %3)
	(set-dst r7, r6, r2)
	(map-src-to-dst)
	sys_call sys_mprotect + 0x2000000
	pop_trashed
%endmacro

%macro sys_munmap 2
	;addr, len
	push_trashed
	(set-src %1, %2)
	(set-dst r7, r6)
	(map-src-to-dst)
	sys_call sys_munmap + 0x2000000
	pop_trashed
%endmacro

%macro sys_stat 2
	;path, buffer
	push_trashed
	(set-src %1, %2)
	(set-dst r7, r6)
	(map-src-to-dst)
	sys_call sys_stat + 0x2000000
	pop_trashed
%endmacro

%macro sys_open 3
	;path, flags, mode
	push_trashed
	(set-src %1, %2, %3)
	(set-dst r7, r6, r2)
	(map-src-to-dst)
	sys_call sys_open + 0x2000000
	pop_trashed
%endmacro

%macro sys_close 1
	;fd
	push_trashed
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	sys_call sys_close + 0x2000000
	pop_trashed
%endmacro

%macro sys_ftruncate 2
	;fd, offset
	push_trashed
	(set-src %1, %2)
	(set-dst r7, r6)
	(map-src-to-dst)
	sys_call sys_ftruncate + 0x2000000
	pop_trashed
%endmacro

%macro sys_unlink 1
	;name
	push_trashed
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	sys_call sys_unlink + 0x2000000
	pop_trashed
%endmacro

%macro sys_gettimeofday 2
	;time, timezone
	push_trashed
	vp_push %1
	(set-src %1, %2)
	(set-dst r7, r6)
	(map-src-to-dst)
	sys_call sys_gettimeofday + 0x2000000
	vp_pop r1
	(vp-cpy-ri r0 r1 timeval_sec)
	(vp-cpy-ri r2 r1 timeval_usec)
	pop_trashed
%endmacro

%elifidn OS, Linux

;;;;;;;;;;;;;;;;;;;
; syscall for Linux
;;;;;;;;;;;;;;;;;;;

(equate 'sys_exit 0x3c)
(equate 'sys_read 0x0)
(equate 'sys_write 0x1)
(equate 'sys_open 0x2)
(equate 'sys_close 0x3)
(equate 'sys_unlink 0x57)
(equate 'sys_ftruncate 0x4d)
(equate 'sys_stat 0x4)
(equate 'sys_mmap 0x9)
(equate 'sys_munmap 0xb)
(equate 'sys_mprotect 0xa)
(equate 'sys_gettimeofday 0x60)

(equate 'prot_none 0x0)
(equate 'prot_read 0x1)
(equate 'prot_write 0x2)
(equate 'prot_exec 0x4)

(equate 'map_shared 0x1)
(equate 'map_private 0x2)
(equate 'map_fixed 0x10)
(equate 'map_anon 0x20)

(equate 'o_rdonly 0x0)
(equate 'o_wronly 0x1)
(equate 'o_rdwr 0x2)
(equate 'o_trunc 0x200)
(equate 'o_append 0x400)
(equate 'o_nonblock 0x800)
(equate 'o_creat 0x40)
(equate 'o_excl 0x80)
(equate 'o_nofollow 0x20000)
(equate 'o_cloexec 0x80000)

(equate 's_irwxu 0x1c0)
(equate 's_irusr 0x100)
(equate 's_iwusr 0x80)
(equate 's_ixusr 0x40)
(equate 's_irwxg 0x38)
(equate 's_irgrp 0x20)
(equate 's_iwgrp 0x10)
(equate 's_ixgrp 0x8)
(equate 's_irwxo 0x7)
(equate 's_iroth 0x4)
(equate 's_iwoth 0x2)
(equate 's_ixoth 0x1)
(equate 's_isuid 0x800)
(equate 's_isgid 0x400)
(equate 's_isvtx 0x200)

(equate 's_ifmt 0xf000)
(equate 's_ifdir 0x4000)
(equate 's_ifreg 0x8000)

(equate 'stat_mode 0x18)
(equate 'stat_fsize 0x30)
(equate 'stat_size 0x100)

(def-struct 'timeval)
	(long 'timeval_sec)
	(long 'timeval_usec)
(def-struct-end)

(def-struct 'timezone)
	int	timeval_minuteswest
	int	timeval_dsttime
(def-struct-end)

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
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	sys_call sys_exit
%endmacro

%macro sys_read 3
	;fd, buffer, length
	push_trashed
	(set-src %1, %2, %3)
	(set-dst r7, r6, r2)
	(map-src-to-dst)
	sys_call sys_read
	pop_trashed
%endmacro

%macro sys_read_char 1
	;fd
	push_trashed
	vp_push 0
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	(vp-cpy-rr r4 r6)
	vp_cpy 1, r2
	sys_call sys_read
	vp_pop r0
	pop_trashed
%endmacro

%macro sys_write_string 3
	;fd, string, length
	push_trashed
	vp_push r0
	(set-src %1, %2, %3)
	(set-dst r7, r6, r2)
	(map-src-to-dst)
	sys_call sys_write
	vp_pop r0
	pop_trashed
%endmacro

%macro sys_write_char 2
	;fd, char
	push_trashed
	vp_push r0, %2
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	(vp-cpy-rr r4 r6)
	vp_cpy 1, r2
	sys_call sys_write
	(vp-add-cr 8 r4)
	vp_pop r0
	pop_trashed
%endmacro

%macro sys_mmap 6
	;addr, len, prot, flags, fd, pos
	push_trashed
	(set-src %1, %2, %3, %4, %5, %6)
	(set-dst r7, r6, r2, r10, r8, r9)
	(map-src-to-dst)
	sys_call sys_mmap
	pop_trashed
%endmacro

%macro sys_mprotect 3
	;addr, len, prot
	push_trashed
	(set-src %1, %2, %3)
	(set-dst r7, r6, r2)
	(map-src-to-dst)
	sys_call sys_mprotect
	pop_trashed
%endmacro

%macro sys_munmap 2
	;addr, len
	push_trashed
	(set-src %1, %2)
	(set-dst r7, r6)
	(map-src-to-dst)
	sys_call sys_munmap
	pop_trashed
%endmacro

%macro sys_stat 2
	;path, buffer
	push_trashed
	(set-src %1, %2)
	(set-dst r7, r6)
	(map-src-to-dst)
	sys_call sys_stat
	pop_trashed
%endmacro

%macro sys_open 3
	;path, flags, mode
	push_trashed
	(set-src %1, %2, %3)
	(set-dst r7, r6, r2)
	(map-src-to-dst)
	sys_call sys_open
	pop_trashed
%endmacro

%macro sys_close 1
	;fd
	push_trashed
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	sys_call sys_close
	pop_trashed
%endmacro

%macro sys_ftruncate 2
	;fd, offset
	push_trashed
	(set-src %1, %2)
	(set-dst r7, r6)
	(map-src-to-dst)
	sys_call sys_ftruncate
	pop_trashed
%endmacro

%macro sys_unlink 1
	;name
	push_trashed
	(set-src %1)
	(set-dst r7)
	(map-src-to-dst)
	sys_call sys_unlink
	pop_trashed
%endmacro

%macro sys_gettimeofday 2
	;time, timezone
	push_trashed
	(set-src %1, %2)
	(set-dst r7, r6)
	(map-src-to-dst)
	sys_call sys_gettimeofday
	pop_trashed
%endmacro
%endif
