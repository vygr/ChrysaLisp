(report-header "System: vpx, named locals kept in registers, lib/asm/vpxreg.inc")

(import "lib/asm/vpxreg.inc")

;a function as it is assembled, from where it is entered: a frame of two
;locals, a and b, and a struct s. What it may trash is :r1 to :r14, it
;takes and gives in :r0
(defq vr_frames '((1 32 ((a 0 "p") (b 8 "p") (s 16 ""))))
	vr_mine '(:r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14) vr_fixed '(:r0))

(defun vr-keep (insts &optional callee mine)
	;what (keep-locals) makes of the instructions, in a frame of a, b and s
	(keep-locals (cat '((emit-alloc 32)) insts '((emit-free 32) (emit-ret))) vr_frames
		(ifn mine vr_mine) vr_fixed (ifn callee (lambda (inst) :nil))))

(defun vr-body (got)
	;the instructions it gave, less the alloc, the free and the ret
	(if got (slice (first got) 1 -3)))

;a local written and read twice, nothing called
(defq vr_got (vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-cpy-ir :rsp 0 :r1) (emit-cpy-ir :rsp 0 :r2))))
(assert-list-eq "written and read twice: it is copied to a register no instruction names, and read from it; the stack is not written"
	'((emit-cpy-rr :r0 :r3) (emit-cpy-rr :r3 :r1) (emit-cpy-rr :r3 :r2)) (vr-body vr_got))
(assert-list-eq "what was done: the local, its home, two reads of the stack gone and one write"
	'(("1:a" :r3 2 1)) (second vr_got))

;a call between
(defq vr_call '((emit-cpy-ri :r0 :rsp 0) (emit-call-p _l_rl_0) (emit-cpy-ir :rsp 0 :r1)))
(assert-eq "a call that does not say what it trashes is between the write and the read: nothing is done"
	:nil (vr-keep vr_call (lambda (inst) (list :nil '(:r0)))))
(assert-eq "a call of what is not known at all: nothing is done" :nil (vr-keep vr_call))
(assert-list-eq "a call that trashes :r1 and :r2 only: the local is kept over it, in a register it leaves"
	'((emit-cpy-rr :r0 :r3) (emit-call-p _l_rl_0) (emit-cpy-rr :r3 :r1))
	(vr-body (vr-keep vr_call (lambda (inst) (list '(:r1 :r2) '(:r0))))))
(assert-list-eq "a call that takes a value in :r2 and trashes :r3: neither is its home"
	'((emit-cpy-rr :r0 :r4) (emit-call-p _l_rl_0) (emit-cpy-rr :r4 :r1))
	(vr-body (vr-keep vr_call (lambda (inst) (list '(:r3) '(:r0 :r2))))))
(assert-list-eq "read before a call that trashes all, and twice after: the first after is of the stack, by way of the home, the second of the home; the write stays"
	'((emit-cpy-ri :r0 :rsp 0) (emit-call-p _l_rl_0) (emit-cpy-ir :rsp 0 :r3) (emit-cpy-rr :r3 :r1) (emit-cpy-rr :r3 :r2))
	(vr-body (vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-call-p _l_rl_0) (emit-cpy-ir :rsp 0 :r1) (emit-cpy-ir :rsp 0 :r2))
		(lambda (inst) (list :nil '(:r0))))))

(assert-list-eq "read from the home, then a call that trashes all, then read again: the second is of the stack, so the write to the stack stays"
	'((emit-cpy-ri :r0 :rsp 0) (emit-cpy-rr :r0 :r3) (emit-cpy-rr :r3 :r1) (emit-call-p _l_rl_0) (emit-cpy-ir :rsp 0 :r2))
	(vr-body (vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-cpy-ir :rsp 0 :r1) (emit-call-p _l_rl_0) (emit-cpy-ir :rsp 0 :r2))
		(lambda (inst) (list :nil '(:r0))))))

;two ways to a read
(assert-eq "had by one way to a read and not by the other, a call that trashes all: the read is of the stack, and nothing is saved, so nothing is done"
	:nil (vr-keep '((emit-cpy-ir :rsp 0 :r1) (emit-beq-cr 0 :r1 _l_skip) (emit-call-p _l_rl_0)
		(emit-label '_l_skip) (emit-cpy-ir :rsp 0 :r2)) (lambda (inst) (list :nil '(:r0)))))
(assert-list-eq "had by both ways: written on each, read after they meet from the home, and the stack never written"
	'((emit-beq-cr 0 :r0 _l_else) (emit-cpy-rr :r0 :r2) (emit-jmp _l_end) (emit-label '_l_else) (emit-cpy-rr :r1 :r2)
		(emit-label '_l_end) (emit-cpy-rr :r2 :r0))
	(vr-body (vr-keep '((emit-beq-cr 0 :r0 _l_else) (emit-cpy-ri :r0 :rsp 0) (emit-jmp _l_end) (emit-label '_l_else)
		(emit-cpy-ri :r1 :rsp 0) (emit-label '_l_end) (emit-cpy-ir :rsp 0 :r0)))))
(assert-list-eq "round a loop: written before it, read and written in it, with no call: all in the home"
	'((emit-cpy-rr :r0 :r2) (emit-label '_l_loop) (emit-cpy-rr :r2 :r1) (emit-add-cr 1 :r1) (emit-cpy-rr :r1 :r2)
		(emit-bne-cr 9 :r1 _l_loop))
	(vr-body (vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-label '_l_loop) (emit-cpy-ir :rsp 0 :r1) (emit-add-cr 1 :r1)
		(emit-cpy-ri :r1 :rsp 0) (emit-bne-cr 9 :r1 _l_loop)))))

;what is not touched
(assert-list-eq "a local whose address is taken stays on the stack, one below it in the frame is kept"
	'((emit-cpy-ri :r0 :rsp 8) (emit-lea-i :rsp 8 :r1) (emit-cpy-rr :r0 :r3) (emit-cpy-rr :r3 :r2))
	(vr-body (vr-keep '((emit-cpy-ri :r0 :rsp 8) (emit-lea-i :rsp 8 :r1) (emit-cpy-ri :r0 :rsp 0) (emit-cpy-ir :rsp 0 :r2)))))
(assert-eq "and one above it is not: what was given the address may fill that too" :nil
	(vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-lea-i :rsp 0 :r1) (emit-cpy-ri :r0 :rsp 8) (emit-cpy-ir :rsp 8 :r2))))
(assert-eq "one read as an int, less than a long: not kept" :nil
	(vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-cpy-ir-i :rsp 0 :r1) (emit-cpy-ir-i :rsp 0 :r2))))
(assert-eq "a struct, and a place part way in to it: not kept" :nil
	(vr-keep '((emit-cpy-ri :r0 :rsp 16) (emit-cpy-ir :rsp 16 :r1) (emit-cpy-ri :r0 :rsp 24) (emit-cpy-ir :rsp 24 :r1))))
(assert-eq "a push with the frame there: the function is not followed" :nil
	(vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-push :r1) (emit-pop :r1) (emit-cpy-ir :rsp 0 :r1) (emit-cpy-ir :rsp 0 :r2))))
(assert-eq "the stack pointer used as a number: not followed" :nil
	(vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-cpy-rr :rsp :r1) (emit-cpy-ir :rsp 0 :r1) (emit-cpy-ir :rsp 0 :r2))))
(assert-eq "a label nothing is seen to go to: not followed" :nil
	(vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-jmp _l_end) (emit-label '_l_lost) (emit-cpy-ir :rsp 0 :r1)
		(emit-label '_l_end) (emit-cpy-ir :rsp 0 :r1) (emit-cpy-ir :rsp 0 :r2))))
(assert-eq "a call by a register: not followed" :nil
	(vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-call-r :r1) (emit-cpy-ir :rsp 0 :r1) (emit-cpy-ir :rsp 0 :r2))))
(assert-eq "no register it may trash that it does not name: nothing is done" :nil
	(vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-cpy-ir :rsp 0 :r1) (emit-cpy-ir :rsp 0 :r2)) :nil '(:r1 :r2)))
(assert-list-eq "one register for two locals: the one read most has it"
	'(("1:b" :r3 3 1))
	(second (vr-keep '((emit-cpy-ri :r0 :rsp 0) (emit-cpy-ri :r0 :rsp 8) (emit-cpy-ir :rsp 0 :r1) (emit-cpy-ir :rsp 0 :r2)
		(emit-cpy-ir :rsp 8 :r1) (emit-cpy-ir :rsp 8 :r2) (emit-cpy-ir :rsp 8 :r1)) :nil '(:r1 :r2 :r3))))
