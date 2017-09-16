(import 'sys/code.inc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; system call register saves
; and sys-call macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
	((eql *os* 'Darwin)
		(defcfun sys-call (_) (vp-syscall (add _ 0x2000000)))
		(cond
			((eql *cpu* 'x86_64)
				(defcmacro sys-args (_ &optional o) (setd o 0) `',(slice o (add o _) '(r6 r5 r2 r9 r7 r8)))
				(defcmacro sys-push-trashed () '(vp-push r1 r2 r5 r6 r7 r8 r9 r10))
				(defcmacro sys-pop-trashed () '(vp-pop r1 r2 r5 r6 r7 r8 r9 r10)))
			(t (throw "Unknown CPU for Darwin !" *cpu*))))
	((eql *os* 'Linux)
		(defcfun sys-call (_) (vp-syscall _))
		(cond
			((eql *cpu* 'x86_64)
				(defcmacro sys-args (_ &optional o) (setd o 0) `',(slice o (add o _) '(r6 r5 r2 r9 r7 r8)))
				(defcmacro sys-push-trashed () '(vp-push r1 r2 r5 r6 r7 r8 r9 r10))
				(defcmacro sys-pop-trashed () '(vp-pop r1 r2 r5 r6 r7 r8 r9 r10)))
			((eql *cpu* 'ARM)
				(defcmacro sys-args (_ &optional o) (setd o 0) `',(slice o (add o _) '(r0 r1 r2 r3 r4 r5)))
				(defcmacro sys-push-trashed () '(vp-push r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
				(defcmacro sys-pop-trashed () '(vp-pop r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14)))
			(t (throw "Unknown CPU for Linux !" *cpu*)))))

;;;;;;;;;;;;;;;;
; syscall macros
;;;;;;;;;;;;;;;;

(when (eql *os* 'Darwin)
	(defcfun sys-gettimeofday (_)
		;time, timezone
		(assign _ (sys-args 2))
		(vp-push (defq _1 (elem 0 (sys-args 1))))
		(vp-xor-rr (defq _2 (elem 0 (sys-args 1 2))) _2)
		(sys-call sys_gettimeofday)
		(vp-pop _1)
		(vpif '(r0 != 0))
			(vp-cpy-ri r0 _1 timeval_sec)
			(vp-cpy-ri _2 _1 timeval_usec)
		(endif)))

(when (eql *os* 'Linux)
	(defcfun sys-gettimeofday (_)
		;time, timezone
		(assign _ (sys-args 2))
		(sys-call sys_gettimeofday)))

(def-struct 'timeval)
	(long 'sec)
	(long 'usec)
(def-struct-end)

(def-struct 'timezone)
	(int 'minuteswest)
	(int 'dsttime)
(def-struct-end)

(defcfun sys-exit (_)
	;return code
	(assign _ (sys-args 1))
	(sys-call sys_exit))

(defcfun sys-read (_1 _2)
	;fd, buffer, length
	(assign _1 (sys-args 3))
	(sys-call sys_read)
	(assign '(r0) _2))

(defcfun sys-read-char (_1 _2)
	;fd
	(assign _1 (sys-args 1))
	(vp-xor-rr (defq _ (elem 0 (sys-args 1 1))) _)
	(vp-push _)
	(assign '(rsp 1) (sys-args 2 1))
	(sys-call sys_read)
	(vp-pop r0)
	(assign '(r0) _2))

(defcfun sys-write (_1 _2)
	;fd, string, length
	(assign _1 (sys-args 3))
	(sys-call sys_write)
	(assign '(r0) _2))

(defcfun sys-write-char (_1 _2)
	;fd, char
	(assign _1 (sys-args 2))
	(vp-push (elem 0 (sys-args 1 1)))
	(assign '(rsp 1) (sys-args 2 1))
	(sys-call sys_write)
	(vp-free ptr_size)
	(assign '(r0) _2))

(defcfun sys-mmap (_1 _2)
	;addr, len, prot, flags, fd, pos
	(assign _1 (sys-args 6))
	(sys-call sys_mmap)
	(assign '(r0) _2))

(defcfun sys-mprotect (_1 _2)
	;addr, len, prot
	(assign _1 (sys-args 3))
	(sys-call sys_mprotect)
	(assign '(r0) _2))

(defcfun sys-munmap (_1 _2)
	;addr, len
	(assign _1 (sys-args 2))
	(sys-call sys_munmap)
	(assign '(r0) _2))

(defcfun sys-stat (_1 _2)
	;path, buffer
	(assign _1 (sys-args 2))
	(sys-call sys_stat)
	(assign '(r0) _2))

(defcfun sys-open (_1 _2)
	;path, flags, mode
	(assign _1 (sys-args 3))
	(sys-call sys_open)
	(assign '(r0) _2))

(defcfun sys-close (_1 _2)
	;fd
	(assign _1 (sys-args 1))
	(sys-call sys_close)
	(assign '(r0) _2))

(defcfun sys-ftruncate (_1 _2)
	;fd, offset
	(assign _1 (sys-args 2))
	(sys-call sys_ftruncate)
	(assign '(r0) _2))

(defcfun sys-unlink (_1 _2)
	;name
	(assign _1 (sys-args 1))
	(sys-call sys_unlink)
	(assign '(r0) _2))

(defcfun sys-fcntl (_1 _2)
	;fd, cmd, arg
	(assign _1 (sys-args 3))
	(sys-call sys_fcntl)
	(assign '(r0) _2))
