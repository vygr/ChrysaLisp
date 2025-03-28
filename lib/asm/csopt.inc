;;;;;;;;;;;;;;;;;;;;
; C-Script optimizer
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq
	+opt_nop
		'(lambda)
	+opt_sxx_cr_ops
		''(vp-seq-cr vp-sne-cr vp-sle-cr vp-sge-cr vp-sgt-cr vp-slt-cr)
	+opt_sxx_rr_ops
		''(vp-seq-rr vp-sne-rr vp-sle-rr vp-sge-rr vp-sgt-rr vp-slt-rr)
	+opt_read_ops
		''(vp-cpy-ir vp-cpy-ir-b vp-cpy-ir-s vp-cpy-ir-i vp-cpy-ir-ub vp-cpy-ir-us vp-cpy-ir-ui)
	+opt_write_ops
		''(vp-cpy-ri vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-i)
	+opt_rr_ops
		''(vp-add-rr vp-sub-rr vp-mul-rr vp-and-rr vp-or-rr vp-xor-rr vp-shl-rr vp-shr-rr vp-asr-rr)
	+opt_cr_ops
		''(vp-add-cr vp-sub-cr vp-mul-cr vp-and-cr vp-or-cr vp-xor-cr vp-shl-cr vp-shr-cr vp-asr-cr)
	+opt_fold_cpy_ops
		''(+ - * logand logior logxor << >> >>>)
	+opt_fold_cr_ops
		''(+ + * logand logior logxor + + +)
	+opt_cr_fuse_ops
		''(vp-add-cr vp-sub-cr))

(defmacro uses? (r op)
	`(rfind ,r ,op))

(defmacro find-past (&rest body)
	`(and (defq _ (some! (lambda (op) (cond ~body))
		(list inst_list) :nil _ 1)) (> _ -1) _))

(defun opt-find-1 (_ r x)
	(find-past
		((lmatch? op x) (!))
		((uses? r op) -1)))

(defun opt-find-2 (_ r x y)
	(find-past
		((lmatch? op x) (setq m 0) (!))
		((lmatch? op y) (setq m 1) (!))
		((uses? r op) -1)))

(defun opt-inst-list (inst_list)
	(defq _ 0 m :nil)
	(while (< (setq _ (inc _)) (length inst_list))
		(cond
			;variable loading and offset folding
			((rfind (defq op (elem-get inst_list _) o (first op)) +opt_read_ops)
				(when (defq p (opt-find-2 _ (defq r (second op)) `(vp-lea-i :rsp _ ,r) `(vp-add-cr _ ,r)))
					(cond
						((= m 0)
							(elem-set op 1 :rsp)
							(elem-set op 2 `(+ ,(third (elem-get inst_list p)) ,(third op))))
						((= m 1)
							(elem-set op 2 `(+ ,(second (elem-get inst_list p)) ,(third op)))
							(setq _ (dec _))))
					(elem-set inst_list p +opt_nop)))
			;variable writing and offset folding
			((rfind o +opt_write_ops)
				(when (defq p (opt-find-2 _ (defq r (third op)) `(vp-lea-i :rsp _ ,r) `(vp-add-cr _ ,r)))
					(cond
						((= m 0)
							(elem-set op 2 :rsp)
							(elem-set op 3 `(+ ,(third (elem-get inst_list p)) ,(elem-get op 3))))
						((= m 1)
							(elem-set op 3 `(+ ,(second (elem-get inst_list p)) ,(elem-get op 3)))
							(setq _ (dec _))))
					(elem-set inst_list p +opt_nop)))
			;strength reduction
			((and (eql o 'vp-mul-cr) (defq s (log2 (eval (second op)))))
				(elem-set op 0 'vp-shl-cr)
				(elem-set op 1 s)
				(setq _ (dec _)))
			;constant propagation
			((defq c (rfind o +opt_rr_ops))
				(when (defq p (opt-find-1 _ (defq r (second op)) `(vp-cpy-cr _ ,r)))
					(elem-set op 0 (elem-get +opt_cr_ops c))
					(elem-set op 1 (second (elem-get inst_list p)))
					(elem-set inst_list p +opt_nop)
					(setq _ (dec _))))
			;arithmetic reassignment and constant folding
			((and (defq c (rfind o +opt_cr_ops))
					(defq p (opt-find-2 _ (defq r (third op)) `(vp-cpy-cr _ ,r) `(,o _ ,r))))
				(cond
					((= m 0)
						(setq c (elem-get +opt_fold_cpy_ops c))
						(elem-set op 0 'vp-cpy-cr))
					((= m 1)
						(setq c (elem-get +opt_fold_cr_ops c))))
				(elem-set op 1 (list c (second (elem-get inst_list p)) (second op)))
				(elem-set inst_list p +opt_nop))
			;constant fusion
			((and (defq c (rfind o +opt_cr_fuse_ops))
					(defq p (opt-find-2 _ (defq r (third op)) `(vp-add-cr _ ,r) `(vp-sub-cr _ ,r))))
				(cond
					((= m 0)
						(setq c (elem-get '(+ -) c)))
					((= m 1)
						(setq c (elem-get '(- +) c))))
				(elem-set op 1 (list c (second op) (second (elem-get inst_list p))))
				(elem-set inst_list p +opt_nop))
			;compare constant forwarding
			((and (defq c (rfind o +opt_sxx_rr_ops))
					(defq p (opt-find-1 _ (defq r (second op)) `(vp-cpy-cr _ ,r)))
					(<= -0x80000000 (defq v (eval (second (elem-get inst_list p)))) 0x7fffffff))
				(elem-set op 0 (elem-get +opt_sxx_cr_ops c))
				(elem-set op 1 v)
				(elem-set inst_list p +opt_nop))
			)))

;module
(export-symbols '(opt-inst-list))
(env-pop)
