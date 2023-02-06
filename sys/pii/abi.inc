;;;;;;;;;;;;;;;;;;;;
; host abi call info
;;;;;;;;;;;;;;;;;;;;

(case *abi*
(AMD64
	(case *cpu*
		(x86_64
			(defmacro abi-trashed () ''(:r1 :r2 :r5 :r6 :r7 :r8 :r9 :r10))
			(defun abi-args (&optional _ o) (setd _ 6 o 0) (slice o (+ o _) '(:r6 :r5 :r2 :r1 :r7 :r8 :r0 :r3 :r4))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(ARM64
	(case *cpu*
		(arm64
			(defmacro abi-trashed () ''(:r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
			(defun abi-args (&optional _ o) (setd _ 8 o 0) (slice o (+ o _) '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(RISCV64
	(case *cpu*
		(riscv64
			(defmacro abi-trashed () ''(:r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8))
			(defun abi-args (&optional _ o) (setd _ 8 o 0) (slice o (+ o _) '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(WIN64
	(case *cpu*
		(x86_64
			(defmacro abi-trashed () ''(:r1 :r2 :r7 :r8 :r9 :r10))
			(defun abi-args (&optional _ o) (setd _ 4 o 0) (slice o (+ o _) '(:r1 :r2 :r7 :r8 :r0 :r3 :r4 :r5 :r6))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(VP64
	(case *cpu*
		(vp64
			(defmacro abi-trashed () ''())
			(defun abi-args (&optional _ o) (setd _ 15 o 0) (slice o (+ o _) '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(:t (throw "Unknown ABI !" *abi*)))

(defmacro abi-push-trashed (&rest b) (merge-obj b (abi-trashed)) `(vp-push ~b))
(defmacro abi-pop-trashed (&rest b) (merge-obj b (abi-trashed)) `(vp-pop ~b))
