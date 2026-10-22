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

;The loader, called by the host, is the reverse, (vp-entry-abi)
