(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;;;;;;
; syscall static class
;;;;;;;;;;;;;;;;;;;;;;

(def-class 'sys_io)
(def-method 'open 'sys/io_open 'static '(r0 r1 r2) '(r0))
(def-method 'close 'sys/io_close 'static '(r0))
(def-method 'char 'sys/io_write_char 'static '(r0 r1))
(def-method 'string 'sys/io_write_str 'static '(r0 r1))
(def-method 'number 'sys/io_write_num 'static '(r0 r1 r2))
(def-method 'read 'sys/io_read_line 'static '(r0 r1 r2) '(r0))
(def-method 'debug_long 'sys/io_debug_long 'static '(r0 r1 r2 r3))
(def-method 'debug_str 'sys/io_debug_str 'static '(r0 r1 r2 r3))

(def-class 'sys_cpu)
(def-method 'id 'sys/cpu_get_id 'static '() '(r0))
(def-method 'time 'sys/cpu_get_time 'static '() '(r0))
(def-method 'total 'sys/cpu_get_total 'static '() '(r0))
(def-method 'opts 'sys/opt_process 'static '(r0))

(defcfun sys-call (%1)
	(vp-cpy-cr %1 r0)
	(vp-syscall))

(when (eql *os* 'Darwin)

;;;;;;;;;;;;;;;;;
; syscall for OSX
;;;;;;;;;;;;;;;;;

(defcvar 'sys_exit 0x1)
(defcvar 'sys_read 0x3)
(defcvar 'sys_write 0x4)
(defcvar 'sys_open 0x5)
(defcvar 'sys_close 0x6)
(defcvar 'sys_unlink 0xa)
(defcvar 'sys_ftruncate 0xc9)
(defcvar 'sys_stat 0xbc)
(defcvar 'sys_mmap 0xc5)
(defcvar 'sys_munmap 0x49)
(defcvar 'sys_mprotect 0x4a)
(defcvar 'sys_gettimeofday 0x74)

(defcvar 'prot_none 0x0)
(defcvar 'prot_read 0x1)
(defcvar 'prot_write 0x2)
(defcvar 'prot_exec 0x4)

(defcvar 'map_shared 0x1)
(defcvar 'map_private 0x2)
(defcvar 'map_fixed 0x10)
(defcvar 'map_anon 0x1000)

(defcvar 'o_rdonly 0x0)
(defcvar 'o_wronly 0x1)
(defcvar 'o_rdwr 0x2)
(defcvar 'o_trunc 0x400)
(defcvar 'o_append 0x8)
(defcvar 'o_nonblock 0x4)
(defcvar 'o_creat 0x200)
(defcvar 'o_excl 0x800)
(defcvar 'o_nofollow 0x100)
(defcvar 'o_cloexec 0x1000000)

(defcvar 's_irwxu 0x1c0)
(defcvar 's_irusr 0x100)
(defcvar 's_iwusr 0x80)
(defcvar 's_ixusr 0x40)
(defcvar 's_irwxg 0x38)
(defcvar 's_irgrp 0x20)
(defcvar 's_iwgrp 0x10)
(defcvar 's_ixgrp 0x8)
(defcvar 's_irwxo 0x7)
(defcvar 's_iroth 0x4)
(defcvar 's_iwoth 0x2)
(defcvar 's_ixoth 0x1)
(defcvar 's_isuid 0x800)
(defcvar 's_isgid 0x400)
(defcvar 's_isvtx 0x200)

(defcvar 's_ifmt 0xf000)
(defcvar 's_ifdir 0x4000)
(defcvar 's_ifreg 0x8000)

(defcvar 'stat_mode 0x8)
(defcvar 'stat_fsize 0x48)
(defcvar 'stat_size 0x100)

(def-struct 'timeval)
	(long 'timeval_sec)
	(long 'timeval_usec)
(def-struct-end)

(def-struct 'timezone)
	(int 'timeval_minuteswest)
	(int 'timeval_dsttime)
(def-struct-end)

;;;;;;;;;;;;;;;;
; syscall macros
;;;;;;;;;;;;;;;;

(defcfun push-trashed ()
	;pushes onto r4
	(vp-push r1 r2 r6 r7 r8 r9 r10 r11))

(defcfun pop-trashed ()
	;pops from r4
	(vp-pop r1 r2 r6 r7 r8 r9 r10 r11))

(defcfun sys-exit (%1)
	;return code
	(assign `(,%1) '(r7))
	(sys-call (add sys_exit 0x2000000)))

(defcfun sys-read (%1 %2 %3)
	;fd, buffer, length
	(push-trashed)
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sys-call (add sys_read 0x2000000))
	(pop-trashed))

(defcfun sys-read-char (%1)
	;fd
	(push-trashed)
	(vp-sub-cr ptr_size r4)
	(assign `(,%1 r4 1) '(r7 r6 r2))
	(sys-call (add sys_read 0x2000000))
	(vp-cpy-ir-ub r4 0 r0)
	(vp-add-cr ptr_size r4)
	(pop-trashed))

(defcfun sys-write-string (%1 %2 %3)
	;fd, string, length
	(push-trashed)
	(vp-push r0)
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sys-call (add sys_write 0x2000000))
	(vp-pop r0)
	(pop-trashed))

(defcfun sys-write-char (%1 %2)
	;fd, char
	(push-trashed)
	(vp-push r0 %2)
	(assign `(,%1) '(r7))
	(vp-cpy-rr r4 r6)
	(vp-cpy-cr 1 r2)
	(sys-call (add sys_write 0x2000000))
	(vp-add-cr ptr_size r4)
	(vp-pop r0)
	(pop-trashed))

(defcfun sys-mmap (%1 %2 %3 %4 %5 %6)
	;addr, len, prot, flags, fd, pos
	(push-trashed)
	(assign `(,%1 ,%2 ,%3 ,%4 ,%5 ,%6) '(r7 r6 r2 r10 r8 r9))
	(sys-call (add sys_mmap 0x2000000))
	(pop-trashed))

(defcfun sys-mprotect (%1 %2 %3)
	;addr, len, prot
	(push-trashed)
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sys-call (add sys_mprotect 0x2000000))
	(pop-trashed))

(defcfun sys-munmap (%1 %2)
	;addr, len
	(push-trashed)
	(assign `(,%1 ,%2) '(r7 r6))
	(sys-call (add sys_munmap 0x2000000))
	(pop-trashed))

(defcfun sys-stat (%1 %2)
	;path, buffer
	(push-trashed)
	(assign `(,%1 ,%2) '(r7 r6))
	(sys-call (add sys_stat 0x2000000))
	(pop-trashed))

(defcfun sys-open (%1 %2 %3)
	;path, flags, mode
	(push-trashed)
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sys-call (add sys_open 0x2000000))
	(pop-trashed))

(defcfun sys-close (%1)
	;fd
	(push-trashed)
	(assign `(,%1) '(r7))
	(sys-call (add sys_close 0x2000000))
	(pop-trashed))

(defcfun sys-ftruncate (%1 %2)
	;fd, offset
	(push-trashed)
	(assign `(,%1 ,%2) '(r7 r6))
	(sys-call (add sys_ftruncate 0x2000000))
	(pop-trashed))

(defcfun sys-unlink (%1)
	;name
	(push-trashed)
	(assign `(,%1) '(r7))
	(sys-call (add sys_unlink 0x2000000))
	(pop-trashed))

(defcfun sys-gettimeofday (%1 %2)
	;time, timezone
	(push-trashed)
	(vp-push %1)
	(assign `(,%1 ,%2) '(r7 r6))
	(sys-call (add sys_gettimeofday 0x2000000))
	(vp-pop r1)
	(vp-cpy-ri r0 r1 timeval_sec)
	(vp-cpy-ri r2 r1 timeval_usec)
	(pop-trashed))

)
(when (eql *os* 'Linux)

;;;;;;;;;;;;;;;;;;;
; syscall for Linux
;;;;;;;;;;;;;;;;;;;

(defcvar 'sys_exit 0x3c)
(defcvar 'sys_read 0x0)
(defcvar 'sys_write 0x1)
(defcvar 'sys_open 0x2)
(defcvar 'sys_close 0x3)
(defcvar 'sys_unlink 0x57)
(defcvar 'sys_ftruncate 0x4d)
(defcvar 'sys_stat 0x4)
(defcvar 'sys_mmap 0x9)
(defcvar 'sys_munmap 0xb)
(defcvar 'sys_mprotect 0xa)
(defcvar 'sys_gettimeofday 0x60)

(defcvar 'prot_none 0x0)
(defcvar 'prot_read 0x1)
(defcvar 'prot_write 0x2)
(defcvar 'prot_exec 0x4)

(defcvar 'map_shared 0x1)
(defcvar 'map_private 0x2)
(defcvar 'map_fixed 0x10)
(defcvar 'map_anon 0x20)

(defcvar 'o_rdonly 0x0)
(defcvar 'o_wronly 0x1)
(defcvar 'o_rdwr 0x2)
(defcvar 'o_trunc 0x200)
(defcvar 'o_append 0x400)
(defcvar 'o_nonblock 0x800)
(defcvar 'o_creat 0x40)
(defcvar 'o_excl 0x80)
(defcvar 'o_nofollow 0x20000)
(defcvar 'o_cloexec 0x80000)

(defcvar 's_irwxu 0x1c0)
(defcvar 's_irusr 0x100)
(defcvar 's_iwusr 0x80)
(defcvar 's_ixusr 0x40)
(defcvar 's_irwxg 0x38)
(defcvar 's_irgrp 0x20)
(defcvar 's_iwgrp 0x10)
(defcvar 's_ixgrp 0x8)
(defcvar 's_irwxo 0x7)
(defcvar 's_iroth 0x4)
(defcvar 's_iwoth 0x2)
(defcvar 's_ixoth 0x1)
(defcvar 's_isuid 0x800)
(defcvar 's_isgid 0x400)
(defcvar 's_isvtx 0x200)

(defcvar 's_ifmt 0xf000)
(defcvar 's_ifdir 0x4000)
(defcvar 's_ifreg 0x8000)

(defcvar 'stat_mode 0x18)
(defcvar 'stat_fsize 0x30)
(defcvar 'stat_size 0x100)

(def-struct 'timeval)
	(long 'timeval_sec)
	(long 'timeval_usec)
(def-struct-end)

(def-struct 'timezone)
	(int 'timeval_minuteswest)
	(int 'timeval_dsttime)
(def-struct-end)

;;;;;;;;;;;;;;;;
; syscall macros
;;;;;;;;;;;;;;;;

(defcfun push-trashed ()
	;pushes onto r4
	(vp-push r1 r2 r6 r7 r8 r9 r10 r11))

(defcfun pop-trashed ()
	;pops from r4
	(vp-pop r1 r2 r6 r7 r8 r9 r10 r11))

(defcfun sys-exit (%1)
	;return code
	(assign `(,%1) '(r7))
	(sys-call sys_exit))

(defcfun sys-read (%1 %2 %3)
	;fd, buffer, length
	(push-trashed)
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sys-call sys_read)
	(pop-trashed))

(defcfun sys-read-char (%1)
	;fd
	(push-trashed)
	(vp-sub-cr ptr_size r4)
	(assign `(,%1 r4 1) '(r7 r6 r2))
	(sys-call sys_read)
	(vp-cpy-ir-ub r4 0 r0)
	(vp-add-cr ptr_size r4)
	(pop-trashed))

(defcfun sys-write-string (%1 %2 %3)
	;fd, string, length
	(push-trashed)
	(vp-push r0)
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sys-call sys_write)
	(vp-pop r0)
	(pop-trashed))

(defcfun sys-write-char (%1 %2)
	;fd, char
	(push-trashed)
	(vp-push r0 %2)
	(assign `(,%1) '(r7))
	(vp-cpy-rr r4 r6)
	(vp-cpy-cr 1 r2)
	(sys-call sys_write)
	(vp-add-cr ptr_size r4)
	(vp-pop r0)
	(pop-trashed))

(defcfun sys-mmap (%1 %2 %3 %4 %5 %6)
	;addr, len, prot, flags, fd, pos
	(push-trashed)
	(assign `(,%1 ,%2 ,%3 ,%4 ,%5 ,%6) '(r7 r6 r2 r10 r8 r9))
	(sys-call sys_mmap)
	(pop-trashed))

(defcfun sys-mprotect (%1 %2 %3)
	;addr, len, prot
	(push-trashed)
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sys-call sys_mprotect)
	(pop-trashed))

(defcfun sys-munmap (%1 %2)
	;addr, len
	(push-trashed)
	(assign `(,%1 ,%2) '(r7 r6))
	(sys-call sys_munmap)
	(pop-trashed))

(defcfun sys-stat (%1 %2)
	;path, buffer
	(push-trashed)
	(assign `(,%1 ,%2) '(r7 r6))
	(sys-call sys_stat)
	(pop-trashed))

(defcfun sys-open (%1 %2 %3)
	;path, flags, mode
	(push-trashed)
	(assign `(,%1 ,%2 ,%3) '(r7 r6 r2))
	(sys-call sys_open)
	(pop-trashed))

(defcfun sys-close (%1)
	;fd
	(push-trashed)
	(assign `(,%1) '(r7))
	(sys-call sys_close)
	(pop-trashed))

(defcfun sys-ftruncate (%1 %2)
	;fd, offset
	(push-trashed)
	(assign `(,%1 ,%2) '(r7 r6))
	(sys-call sys_ftruncate)
	(pop-trashed))

(defcfun sys-unlink (%1)
	;name
	(push-trashed)
	(assign `(,%1) '(r7))
	(sys-call sys_unlink)
	(pop-trashed))

(defcfun sys-gettimeofday (%1 %2)
	;time, timezone
	(push-trashed)
	(assign `(,%1 ,%2) '(r7 r6))
	(sys-call sys_gettimeofday)
	(pop-trashed))
)
