(report-header "System: vpx, named locals kept in registers, lib/asm/vpxreg.inc")

(import "lib/asm/vpxreg.inc")

;a function as it is assembled, from where it is entered: a frame of two
;locals, a and b, and a struct s. What it may trash is :r1 to :r14, it
;takes and gives in :r0
(defq vr_frames '((1 32 ((a 0 "p") (b 8 "p") (s 16 ""))))
	vr_mine '(:r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14) vr_fixed '(:r0))

(defun vr-keep (insts &optional callee mine)
	;what (keep-locals) makes of the instructions, in a frame of a, b and s
	(keep-locals (cat '((alloc 32)) insts '((free 32) (ret))) vr_frames
		(ifn mine vr_mine) vr_fixed (ifn callee (lambda (inst) :nil))))

(defun vr-body (got)
	;the instructions it gave, less the alloc, the free and the ret
	(if got (slice (first got) 1 -3)))

;a local written and read twice, nothing called
(defq vr_got (vr-keep '((store-rrc :r0 :rsp 0) (load-rcr :rsp 0 :r1) (load-rcr :rsp 0 :r2))))
(assert-list-eq "written and read twice: it is copied to a register no instruction names, and read from it; the stack is not written"
	'((cpy-rr :r0 :r3) (cpy-rr :r3 :r1) (cpy-rr :r3 :r2)) (vr-body vr_got))
(assert-list-eq "what was done: the local, its home, two reads of the stack gone and one write"
	'(("1:a" :r3 2 1)) (second vr_got))

;a call between
(defq vr_call '((store-rrc :r0 :rsp 0) (call-p _l_rl_0) (load-rcr :rsp 0 :r1)))
(assert-eq "a call that does not say what it trashes is between the write and the read: nothing is done"
	:nil (vr-keep vr_call (lambda (inst) (list :nil '(:r0)))))
(assert-eq "a call of what is not known at all: nothing is done" :nil (vr-keep vr_call))
(assert-list-eq "a call that trashes :r1 and :r2 only: the local is kept over it, in a register it leaves"
	'((cpy-rr :r0 :r3) (call-p _l_rl_0) (cpy-rr :r3 :r1))
	(vr-body (vr-keep vr_call (lambda (inst) (list '(:r1 :r2) '(:r0))))))
(assert-list-eq "a call that takes a value in :r2 and trashes :r3: neither is its home"
	'((cpy-rr :r0 :r4) (call-p _l_rl_0) (cpy-rr :r4 :r1))
	(vr-body (vr-keep vr_call (lambda (inst) (list '(:r3) '(:r0 :r2))))))
(assert-list-eq "read before a call that trashes all, and twice after: the first after is of the stack, by way of the home, the second of the home; the write stays"
	'((store-rrc :r0 :rsp 0) (call-p _l_rl_0) (load-rcr :rsp 0 :r3) (cpy-rr :r3 :r1) (cpy-rr :r3 :r2))
	(vr-body (vr-keep '((store-rrc :r0 :rsp 0) (call-p _l_rl_0) (load-rcr :rsp 0 :r1) (load-rcr :rsp 0 :r2))
		(lambda (inst) (list :nil '(:r0))))))

(assert-list-eq "read from the home, then a call that trashes all, then read again: the second is of the stack, so the write to the stack stays"
	'((store-rrc :r0 :rsp 0) (cpy-rr :r0 :r3) (cpy-rr :r3 :r1) (call-p _l_rl_0) (load-rcr :rsp 0 :r2))
	(vr-body (vr-keep '((store-rrc :r0 :rsp 0) (load-rcr :rsp 0 :r1) (call-p _l_rl_0) (load-rcr :rsp 0 :r2))
		(lambda (inst) (list :nil '(:r0))))))

;two ways to a read
(assert-eq "had by one way to a read and not by the other, a call that trashes all: the read is of the stack, and nothing is saved, so nothing is done"
	:nil (vr-keep '((load-rcr :rsp 0 :r1) (beq-cr 0 :r1 _l_skip) (call-p _l_rl_0)
		(label '_l_skip) (load-rcr :rsp 0 :r2)) (lambda (inst) (list :nil '(:r0)))))
(assert-list-eq "had by both ways: written on each, read after they meet from the home, and the stack never written"
	'((beq-cr 0 :r0 _l_else) (cpy-rr :r0 :r2) (jmp _l_end) (label '_l_else) (cpy-rr :r1 :r2)
		(label '_l_end) (cpy-rr :r2 :r0))
	(vr-body (vr-keep '((beq-cr 0 :r0 _l_else) (store-rrc :r0 :rsp 0) (jmp _l_end) (label '_l_else)
		(store-rrc :r1 :rsp 0) (label '_l_end) (load-rcr :rsp 0 :r0)))))
(assert-list-eq "round a loop: written before it, read and written in it, with no call: all in the home"
	'((cpy-rr :r0 :r2) (label '_l_loop) (cpy-rr :r2 :r1) (add-cr 1 :r1) (cpy-rr :r1 :r2)
		(bne-cr 9 :r1 _l_loop))
	(vr-body (vr-keep '((store-rrc :r0 :rsp 0) (label '_l_loop) (load-rcr :rsp 0 :r1) (add-cr 1 :r1)
		(store-rrc :r1 :rsp 0) (bne-cr 9 :r1 _l_loop)))))

;what is not touched
(assert-list-eq "a local whose address is taken stays on the stack, one below it in the frame is kept"
	'((store-rrc :r0 :rsp 8) (lea-rcr :rsp 8 :r1) (cpy-rr :r0 :r3) (cpy-rr :r3 :r2))
	(vr-body (vr-keep '((store-rrc :r0 :rsp 8) (lea-rcr :rsp 8 :r1) (store-rrc :r0 :rsp 0) (load-rcr :rsp 0 :r2)))))
(assert-eq "and one above it is not: what was given the address may fill that too" :nil
	(vr-keep '((store-rrc :r0 :rsp 0) (lea-rcr :rsp 0 :r1) (store-rrc :r0 :rsp 8) (load-rcr :rsp 8 :r2))))
(assert-eq "one read as an int, less than a long: not kept" :nil
	(vr-keep '((store-rrc :r0 :rsp 0) (load-rcr-i :rsp 0 :r1) (load-rcr-i :rsp 0 :r2))))
(assert-eq "a struct, and a place part way in to it: not kept" :nil
	(vr-keep '((store-rrc :r0 :rsp 16) (load-rcr :rsp 16 :r1) (store-rrc :r0 :rsp 24) (load-rcr :rsp 24 :r1))))
(assert-eq "a push with the frame there: the function is not followed" :nil
	(vr-keep '((store-rrc :r0 :rsp 0) (push :r1) (pop :r1) (load-rcr :rsp 0 :r1) (load-rcr :rsp 0 :r2))))
(assert-eq "the stack pointer used as a number: not followed" :nil
	(vr-keep '((store-rrc :r0 :rsp 0) (cpy-rr :rsp :r1) (load-rcr :rsp 0 :r1) (load-rcr :rsp 0 :r2))))
(assert-eq "a label nothing is seen to go to: not followed" :nil
	(vr-keep '((store-rrc :r0 :rsp 0) (jmp _l_end) (label '_l_lost) (load-rcr :rsp 0 :r1)
		(label '_l_end) (load-rcr :rsp 0 :r1) (load-rcr :rsp 0 :r2))))
(assert-eq "a call by a register: not followed" :nil
	(vr-keep '((store-rrc :r0 :rsp 0) (call-r :r1) (load-rcr :rsp 0 :r1) (load-rcr :rsp 0 :r2))))
(assert-eq "no register it may trash that it does not name: nothing is done" :nil
	(vr-keep '((store-rrc :r0 :rsp 0) (load-rcr :rsp 0 :r1) (load-rcr :rsp 0 :r2)) :nil '(:r1 :r2)))
(assert-list-eq "one register for two locals: the one read most has it"
	'(("1:b" :r3 3 1))
	(second (vr-keep '((store-rrc :r0 :rsp 0) (store-rrc :r0 :rsp 8) (load-rcr :rsp 0 :r1) (load-rcr :rsp 0 :r2)
		(load-rcr :rsp 8 :r1) (load-rcr :rsp 8 :r2) (load-rcr :rsp 8 :r1)) :nil '(:r1 :r2 :r3))))

;the VP optimizer, lib/asm/vpopt.inc, over a list of emits: what it knows of
;memory it must not take for what was last stored there
(import "lib/asm/ops.inc")
(import "lib/asm/vpopt.inc")
(defun vo-opt (&rest emits)
	(defq emit_list (map (# (cat %0)) emits))
	(opt-emit-list emit_list 0 (length emit_list))
	(filter (# (nql (first %0) 'vp-nop)) emit_list))
(assert-list-eq "a load of what was just stored is that register"
	'((store-rrc :r0 :r1 8) (cpy-rr :r0 :r2))
	(vo-opt '(store-rrc :r0 :r1 8) '(load-rcr :r1 8 :r2)))
(assert-list-eq "but not past a float stored there"
	'((store-rrc :r0 :r1 8) (store-drc :d0 :r1 8) (load-rcr :r1 8 :r2))
	(vo-opt '(store-rrc :r0 :r1 8) '(store-drc :d0 :r1 8) '(load-rcr :r1 8 :r2)))
(assert-list-eq "a store nothing reads goes"
	'((store-rrc :r2 :r1 8))
	(vo-opt '(store-rrc :r0 :r1 8) '(store-rrc :r2 :r1 8)))
(assert-list-eq "but not one a float loads"
	'((store-rrc :r0 :r1 8) (load-rcd :r1 8 :d0) (store-rrc :r2 :r1 8))
	(vo-opt '(store-rrc :r0 :r1 8) '(load-rcd :r1 8 :d0) '(store-rrc :r2 :r1 8)))
(assert-list-eq "or a load of another width does"
	'((store-rrc :r0 :r1 8) (load-rcr-ub :r1 8 :r3) (store-rrc :r2 :r1 8))
	(vo-opt '(store-rrc :r0 :r1 8) '(load-rcr-ub :r1 8 :r3) '(store-rrc :r2 :r1 8)))
