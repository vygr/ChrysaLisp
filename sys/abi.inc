(include "sys/code.inc")

;;;;;;;;;;;;;;;;;;;;
; host abi call info
;;;;;;;;;;;;;;;;;;;;

(case *abi*
(AMD64
	(case *cpu*
		(x86_64
			(defmacro abi-trashed () ''(:r1 :r2 :r5 :r6 :r7 :r8 :r9 :r10))
			(defun abi-args (&optional _ o) (setd _ 6 o 0) (slice o (+ o _) '(:r6 :r5 :r2 :r1 :r7 :r8 :r0 :r3 :r4))))
		(t (throw "Unknown CPU for AMD64 !" *cpu*))))
(ARM64
	(case *cpu*
		(aarch64
			(defmacro abi-trashed () ''(:r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
			(defun abi-args (&optional _ o) (setd _ 8 o 0) (slice o (+ o _) '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8))))
		(t (throw "Unknown CPU for ARM64 !" *cpu*))))
(WIN64
	(case *cpu*
		(x86_64
			(defmacro abi-trashed () ''(:r1 :r2 :r7 :r8 :r9 :r10))
			(defun abi-args (&optional _ o) (setd _ 4 o 0) (slice o (+ o _) '(:r1 :r2 :r7 :r8 :r0 :r3 :r4 :r5 :r6))))
		(t (throw "Unknown CPU for WIN64 !" *cpu*))))
(VP64
	(case *cpu*
		(vp64
			(defmacro abi-trashed () ''())
			(defun abi-args (&optional _ o) (setd _ 15 o 0) (slice o (+ o _) '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))))
		(t (throw "Unknown CPU for VP64 !" *cpu*))))
(t (throw "Unknown ABI !" *abi*)))

(defmacro abi-push-trashed (&rest b) (merge-obj b (abi-trashed)) `(vp-push ~b))
(defmacro abi-pop-trashed (&rest b) (merge-obj b (abi-trashed)) `(vp-pop ~b))

(def-enum +file_open 0
	(enum read write append))

(def-enum +mmap 0
	(enum data exec shared none))

(def-struct stat 0
	(long mtime fsize)
	(ushort mode))

(defcvar 's_ifmt 0xf000)
(defcvar 's_ifdir 0x4000)
(defcvar 's_ifreg 0x8000)
