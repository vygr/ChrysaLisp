;;;;;;;;;;;;;;;;;;;;
; C-Script optimizer
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +opt_sxx_cr_ops
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
	`(find-rev ,r ,op))

(defmacro find-past (&rest _)
	`(and (defq _ (some! _ 1 nil
			(lambda (op) (cond ~_))
		(list inst_list))) (> _ -1) _))

(defun opt-find-1 (_ r x)
	(find-past
		((match? op x) _)
		((uses? r op) -1)))

(defun opt-find-2 (_ r x y)
	(find-past
		((match? op x) (setq m 0) _)
		((match? op y) (setq m 1) _)
		((uses? r op) -1)))

(defun opt-inst-list (inst_list)
	(defq _ 0 m nil)
	(while (< (setq _ (inc _)) (length inst_list))
		(cond
			;variable loading and offset folding
			((find-rev (defq op (elem _ inst_list) o (elem 0 op)) +opt_read_ops)
				(when (defq p (opt-find-2 _ (defq r (elem 1 op)) `(vp-lea-i :rsp _ ,r) `(vp-add-cr _ ,r)))
					(cond
						((= m 0)
							(elem-set 1 op :rsp)
							(elem-set 2 op `(+ ,(elem 2 (elem p inst_list)) ,(elem 2 op))))
						((= m 1)
							(elem-set 2 op `(+ ,(elem 1 (elem p inst_list)) ,(elem 2 op)))
							(setq _ (dec _))))
					(elem-set p inst_list (lambda))))
			;variable writing and offset folding
			((find-rev o +opt_write_ops)
				(when (defq p (opt-find-2 _ (defq r (elem 2 op)) `(vp-lea-i :rsp _ ,r) `(vp-add-cr _ ,r)))
					(cond
						((= m 0)
							(elem-set 2 op :rsp)
							(elem-set 3 op `(+ ,(elem 2 (elem p inst_list)) ,(elem 3 op))))
						((= m 1)
							(elem-set 3 op `(+ ,(elem 1 (elem p inst_list)) ,(elem 3 op)))
							(setq _ (dec _))))
					(elem-set p inst_list (lambda))))
			;strength reduction
			((and (eql o 'vp-mul-cr) (defq s (log2 (eval (elem 1 op)))))
				(elem-set 0 op 'vp-shl-cr)
				(elem-set 1 op s)
				(setq _ (dec _)))
			;constant propagation
			((defq c (find-rev o +opt_rr_ops))
				(when (defq p (opt-find-1 _ (defq r (elem 1 op)) `(vp-cpy-cr _ ,r)))
					(elem-set 0 op (elem c +opt_cr_ops))
					(elem-set 1 op (elem 1 (elem p inst_list)))
					(elem-set p inst_list (lambda))
					(setq _ (dec _))))
			;arithmetic reassignment and constant folding
			((and (defq c (find-rev o +opt_cr_ops))
					(defq p (opt-find-2 _ (defq r (elem 2 op)) `(vp-cpy-cr _ ,r) `(,o _ ,r))))
				(cond
					((= m 0)
						(setq c (elem c +opt_fold_cpy_ops))
						(elem-set 0 op 'vp-cpy-cr))
					((= m 1)
						(setq c (elem c +opt_fold_cr_ops))))
				(elem-set 1 op (list c (elem 1 (elem p inst_list)) (elem 1 op)))
				(elem-set p inst_list (lambda)))
			;constant fusion
			((and (defq c (find-rev o +opt_cr_fuse_ops))
					(defq p (opt-find-2 _ (defq r (elem 2 op)) `(vp-add-cr _ ,r) `(vp-sub-cr _ ,r))))
				(cond
					((= m 0)
						(setq c (elem c '(+ -))))
					((= m 1)
						(setq c (elem c '(- +)))))
				(elem-set 1 op (list c (elem 1 op) (elem 1 (elem p inst_list))))
				(elem-set p inst_list (lambda)))
			;compare constant forwarding
			((and (defq c (find-rev o +opt_sxx_rr_ops))
					(defq p (opt-find-1 _ (defq r (elem 1 op)) `(vp-cpy-cr _ ,r)))
					(<= -0x80000000 (defq v (eval (elem 1 (elem p inst_list)))) 0x7fffffff))
				(elem-set 0 op (elem c +opt_sxx_cr_ops))
				(elem-set 1 op v)
				(elem-set p inst_list (lambda)))
			)))

;module
(export-symbols
	opt-inst-list)
(env-pop)
