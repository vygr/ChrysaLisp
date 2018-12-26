(include 'sys/code.inc)

;;;;;;;;;;;;;;;;;;;;
; host abi call info
;;;;;;;;;;;;;;;;;;;;

(cond
	((eql *os* 'Darwin)
		(cond
			((eql *cpu* 'x86_64)
				(defmacro abi-trashed () ''(r1 r2 r5 r6 r7 r8 r9 r10))
				(defcmacro abi-args (&optional _ o) (setd _ 6 o 0) `',(slice o (add o _) '(r6 r5 r2 r1 r7 r8 r0 r3 r4)))
				(defcfun emit-call-abi (r b c &rest x)
					(setq x (cat (list r r) x))
					(emit-cpy-rr rsp r)
					(emit-and-cr -16 rsp)
					(apply emit-push x)
					(emit-call-jump-i 0x10 b c)
					(emit-cpy-ir rsp (mul ptr_size (sub (length x) 2)) rsp)))
			(t (throw "Unknown CPU for Darwin !" *cpu*))))
	((eql *os* 'Linux)
		(cond
			((eql *cpu* 'x86_64)
				(defmacro abi-trashed () ''(r1 r2 r5 r6 r7 r8 r9 r10))
				(defcmacro abi-args (&optional _ o) (setd _ 6 o 0) `',(slice o (add o _) '(r6 r5 r2 r1 r7 r8 r0 r3 r4)))
				(defcfun emit-call-abi (r b c &rest x)
					(setq x (cat (list r r) x))
					(emit-cpy-rr rsp r)
					(emit-and-cr -16 rsp)
					(apply emit-push x)
					(emit-call-jump-i 0x10 b c)
					(emit-cpy-ir rsp (mul ptr_size (sub (length x) 2)) rsp)))
			((eql *cpu* 'aarch64)
				(defmacro abi-trashed () ''(r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
				(defcmacro abi-args (&optional _ o) (setd _ 8 o 0) `',(slice o (add o _) '(r0 r1 r2 r3 r4 r5 r6 r7 r8)))
				(defcfun emit-call-abi (r b c &rest x)
					(setq x (cat '(r30) x))
					(apply emit-push x)
					(emit-cpy-ir b c r29)
					(emit-int (add 0xd63f0000 (bit-shl (reg r29) 5)))
					(cond
						((eq 1 (defq l (length x)))
							(emit-pop r30))
						((eq 2 l) 
							(emit-pop r30 r29))
						(t
							(emit-cpy-ir rsp (mul ptr_size (dec l)) r30)
							(emit-free (mul ptr_size l))))))
			(t (throw "Unknown CPU for Linux !" *cpu*))))
	(t (throw "Unknown OS !" *os*)))

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
	(defcvar 's_ifmt 0xf000)
	(defcvar 's_ifdir 0x4000)
	(defcvar 's_ifreg 0x8000)
)(when (and (eql *os* 'Linux) (eql *cpu* 'x86_64))
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
	(defcvar 's_ifmt 0xf000)
	(defcvar 's_ifdir 0x4000)
	(defcvar 's_ifreg 0x8000)
)(when (and (eql *os* 'Linux) (eql *cpu* 'aarch64))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; syscall for Linux aarch64
	;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defcvar 'prot_none 0x0)
	(defcvar 'prot_read 0x1)
	(defcvar 'prot_write 0x2)
	(defcvar 'prot_exec 0x4)
	(defcvar 'map_shared 0x1)
	(defcvar 'map_private 0x2)
	(defcvar 'map_fixed 0x10)
	(defcvar 'map_anon 0x20)
	(defcvar 's_ifmt 0xf000)
	(defcvar 's_ifdir 0x4000)
	(defcvar 's_ifreg 0x8000)
)

(def-enum 'file_open)
	(enum 'read)
	(enum 'write)
	(enum 'readwrite)
(def-enum-end)

(def-struct 'stat)
	(long 'mtime)
	(long 'fsize)
	(ushort 'mode)
(def-struct-end)
