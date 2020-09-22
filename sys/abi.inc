(include 'sys/code.inc)

;;;;;;;;;;;;;;;;;;;;
; host abi call info
;;;;;;;;;;;;;;;;;;;;

(case *abi*
	(AMD64
		(case *cpu*
			(x86_64
				(defmacro abi-trashed () ''(r1 r2 r5 r6 r7 r8 r9 r10))
				(defcmacro abi-args (&optional _ o) (setd _ 6 o 0) `',(slice o (+ o _) '(r6 r5 r2 r1 r7 r8 r0 r3 r4)))
				(defcfun emit-call-abi (r b c &rest x)
					(cond
						((= *func_align* 16)
							(apply emit-push x)
							(emit-call-jump-i 0x10 b c)
							(emit-add-cr (* ptr_size (length x)) rsp))
						(t
							(setq x (cat (list r r) x))
							(emit-cpy-rr rsp r)
							(emit-and-cr -16 rsp)
							(apply emit-push x)
							(emit-call-jump-i 0x10 b c)
							(emit-cpy-ir rsp (* ptr_size (- (length x) 2)) rsp)))))
			(t (throw "Unknown CPU for AMD64 !" *cpu*))))
	(ARM64
		(case *cpu*
			(aarch64
				(defmacro abi-trashed () ''(r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
				(defcmacro abi-args (&optional _ o) (setd _ 8 o 0) `',(slice o (+ o _) '(r0 r1 r2 r3 r4 r5 r6 r7 r8)))
				(defcfun emit-call-abi (r b c &rest x)
					(setq x (cat '(r30) x))
					(apply emit-push x)
					(emit-cpy-ir b c r29)
					(emit-int (+ 0xd63f0000 (<< (reg r29) 5)))
					(cond
						((= 1 (defq l (length x)))
							(emit-pop r30))
						((= 2 l) 
							(emit-pop r30 r29))
						(t
							(emit-cpy-ir rsp (* ptr_size (dec l)) r30)
							(emit-free (* ptr_size l))))))
			(t (throw "Unknown CPU for ARM64 !" *cpu*))))
	(WIN64
		(case *cpu*
			(x86_64
				(defmacro abi-trashed () ''(r1 r2 r7 r8 r9 r10))
				(defcmacro abi-args (&optional _ o) (setd _ 4 o 0) `',(slice o (+ o _) '(r1 r2 r7 r8 r0 r3 r4 r5 r6)))
				(defcfun emit-call-abi (r b c &rest x)
					(cond
						((= *func_align* 16)
							(apply emit-push x)
							(emit-sub-cr 32 rsp)
							(emit-call-jump-i 0x10 b c)
							(emit-add-cr (* ptr_size (+ (length x) 4)) rsp))
						(t
							(emit-cpy-rr rsp r)
							(emit-and-cr -16 rsp)
							(apply emit-push (cat (list r r) x))
							(emit-sub-cr 32 rsp)
							(emit-call-jump-i 0x10 b c)
							(emit-cpy-ir rsp (* ptr_size (+ (length x) 4)) rsp)))))
			(t (throw "Unknown CPU for WIN64 !" *cpu*))))
	(t (throw "Unknown ABI !" *abi*)))

(defcmacro abi-push-trashed (&rest b) (merge-obj b (abi-trashed)) `(vp-push ~b))
(defcmacro abi-pop-trashed (&rest b) (merge-obj b (abi-trashed)) `(vp-pop ~b))
(defcmacro abi-arg (_) (elem _ (abi-args 6)))

(def-enum 'file_open)
	(enum 'read 'write 'readwrite)
(def-enum-end)

(def-enum 'mmap)
	(enum 'data 'exec 'shared)
(def-enum-end)

(def-enum 'mprotect)
	(enum 'none)
(def-enum-end)

(def-struct 'stat)
	(long 'mtime 'fsize)
	(ushort 'mode)
(def-struct-end)

(defcvar 's_ifmt 0xf000)
(defcvar 's_ifdir 0x4000)
(defcvar 's_ifreg 0x8000)
