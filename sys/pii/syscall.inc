(include 'sys/code.inc)

;;;;;;;;;;;;;;;;;;;;
; host abi call info
;;;;;;;;;;;;;;;;;;;;

(cond
	((eql *os* 'Darwin)
		(cond
			((eql *cpu* 'x86_64)
				(defcmacro abi-args (&optional _ o) (setd _ 6 o 0) `',(slice o (add o _) '(r6 r5 r2 r1 r7 r8 r0 r3 r4)))
				(defmacro abi-trashed () ''(r1 r2 r5 r6 r7 r8 r9 r10)))
			(t (throw "Unknown CPU for Darwin !" *cpu*))))
	((eql *os* 'Linux)
		(cond
			((eql *cpu* 'x86_64)
				(defcmacro abi-args (&optional _ o) (setd _ 6 o 0) `',(slice o (add o _) '(r6 r5 r2 r1 r7 r8 r0 r3 r4)))
				(defmacro abi-trashed () ''(r1 r2 r5 r6 r7 r8 r9 r10)))
			((eql *cpu* 'aarch64)
				(defcmacro abi-args (&optional _ o) (setd _ 8 o 0) `',(slice o (add o _) '(r0 r1 r2 r3 r4 r5 r6 r7 r8)))
				(defmacro abi-trashed () ''(r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14)))
			(t (throw "Unknown CPU for Linux !" *cpu*)))))

(defcmacro abi-push-trashed (&rest b) (merge-sym b (abi-trashed)) `(vp-push ~b))
(defcmacro abi-pop-trashed (&rest b) (merge-sym b (abi-trashed)) `(vp-pop ~b))
(defcmacro abi-arg (_) (elem _ (abi-args 6)))

(when (eql *os* 'Darwin)

;;;;;;;;;;;;;;;;;
; syscall for OSX
;;;;;;;;;;;;;;;;;

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

(defcvar 'f_getfl 0x3)
(defcvar 'f_setfl 0x4)

)
(when (and (eql *os* 'Linux) (eql *cpu* 'x86_64))

;;;;;;;;;;;;;;;;;;;;;;;
; syscall for Linux x64
;;;;;;;;;;;;;;;;;;;;;;;

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

(defcvar 'f_getfl 0x3)
(defcvar 'f_setfl 0x4)

)
(when (and (eql *os* 'Linux) (eql *cpu* 'aarch64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; syscall for Linux aarch64
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defcvar 'o_nofollow 0x8000)
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

(defcvar 'f_getfl 0x3)
(defcvar 'f_setfl 0x4)

)

(def-struct 'stat)
	(long 'mtime)
	(long 'fsize)
	(ushort 'mode)
(def-struct-end)
