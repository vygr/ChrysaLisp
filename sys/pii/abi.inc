;;;;;;;;;;;;;;;;;;;;
; host abi call info
;;;;;;;;;;;;;;;;;;;;

;A call of the host takes its args in :r0 :r1 ... in order, and gives its
;answer in :r0, and keeps every other register, the same VP on every
;platform. The translator of each does the rest in (emit-call-abi): moves
;the args to where its ABI has them, puts on the stack what does not fit,
;and keeps the registers its ABI trashes. So (abi-trashed) is none, and
;(abi-push-trashed) pushes only what it is given.

(defun abi-args (&optional _ o) (setd _ 15 o 0) (slice '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14) o (+ o _)))
(defmacro abi-trashed () ''())
(defmacro abi-push-trashed (&rest b) `(vp-push ~b))
(defmacro abi-pop-trashed (&rest b) `(vp-pop ~b))

;The loader is called by the host, not a call of it, and its args are
;where the host's ABI puts them, (abi-entry-args)

(case *abi*
(AMD64
	(case *cpu*
		(x86_64
			(defun abi-entry-args (&optional _ o) (setd _ 6 o 0) (slice '(:r6 :r5 :r2 :r1 :r7 :r8 :r0 :r3 :r4) o (+ o _))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(ARM64
	(case *cpu*
		(arm64
			(defun abi-entry-args (&optional _ o) (setd _ 8 o 0) (slice '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8) o (+ o _))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(RISCV64
	(case *cpu*
		(riscv64
			(defun abi-entry-args (&optional _ o) (setd _ 8 o 0) (slice '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8) o (+ o _))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(LA64
	(case *cpu*
		(la64
			(defun abi-entry-args (&optional _ o) (setd _ 8 o 0) (slice '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8) o (+ o _))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(WIN64
	(case *cpu*
		(x86_64
			(defun abi-entry-args (&optional _ o) (setd _ 4 o 0) (slice '(:r1 :r2 :r7 :r8 :r0 :r3 :r4 :r5 :r6) o (+ o _))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(VP64
	(case *cpu*
		(vp64
			(defun abi-entry-args (&optional _ o) (setd _ 15 o 0) (slice '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14) o (+ o _))))
		(:t (throw (cat "Unknown CPU for ABI " *abi* " !") *cpu*))))
(:t (throw "Unknown ABI !" *abi*)))

