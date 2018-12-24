(include 'sys/code.inc)

;;;;;;;;;;;;;;;;;;
; system call info
;;;;;;;;;;;;;;;;;;

(cond
	((eql *os* 'Darwin)
		(defcfun sys-call (_) (vp-syscall (add _ 0x2000000)))
		(cond
			((eql *cpu* 'x86_64)
				(defcmacro abi-args (&optional _ o) (setd _ 6 o 0) `',(slice o (add o _) '(r6 r5 r2 r1 r7 r8 r0 r3 r4)))
				(defmacro abi-trashed () ''(r1 r2 r5 r6 r7 r8 r9 r10))
				(defmacro sys-args (_ &optional o) (setd o 0) `',(slice o (add o _) '(r6 r5 r2 r9 r7 r8)))
				(defmacro sys-trashed () ''(r1 r2 r5 r6 r7 r8 r9 r10)))
			(t (throw "Unknown CPU for Darwin !" *cpu*))))
	((eql *os* 'Linux)
		(defcfun sys-call (_) (vp-syscall _))
		(cond
			((eql *cpu* 'x86_64)
				(defcmacro abi-args (&optional _ o) (setd _ 6 o 0) `',(slice o (add o _) '(r6 r5 r2 r1 r7 r8 r0 r3 r4)))
				(defmacro abi-trashed () ''(r1 r2 r5 r6 r7 r8 r9 r10))
				(defmacro sys-args (_ &optional o) (setd o 0) `',(slice o (add o _) '(r6 r5 r2 r9 r7 r8)))
				(defmacro sys-trashed () ''(r1 r2 r5 r6 r7 r8 r9 r10)))
			((eql *cpu* 'aarch64)
				(defcmacro abi-args (&optional _ o) (setd _ 8 o 0) `',(slice o (add o _) '(r0 r1 r2 r3 r4 r5 r6 r7 r8)))
				(defmacro abi-trashed () ''(r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
				(defmacro sys-args (_ &optional o) (setd o 0) `',(slice o (add o _) '(r0 r1 r2 r3 r4 r5)))
				(defmacro sys-trashed () ''(r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14)))
			(t (throw "Unknown CPU for Linux !" *cpu*)))))

(defcmacro abi-push-trashed (&rest b) (merge-sym b (abi-trashed)) `(vp-push ~b))
(defcmacro abi-pop-trashed (&rest b) (merge-sym b (abi-trashed)) `(vp-pop ~b))
(defcmacro abi-arg (_) (elem _ (abi-args 6)))
(defcmacro sys-push-trashed (&rest b) (merge-sym b (sys-trashed)) `(vp-push ~b))
(defcmacro sys-pop-trashed (&rest b) (merge-sym b (sys-trashed)) `(vp-pop ~b))
(defcmacro sys-arg (_) (elem _ (sys-args 6)))

;;;;;;;;;;;;;;;;
; syscall macros
;;;;;;;;;;;;;;;;

(defcfun sys-stat (_1 _2)
	;path, buffer
	(assign (cat '(at_fdcwd) _1 '(0)) (sys-args 4))
	(sys-call sys_fstatat)
	(assign '(r0) _2))
