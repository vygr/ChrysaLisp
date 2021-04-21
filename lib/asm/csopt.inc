;;;;;;;;;;;;;;;;;;;;
; C-Script optimizer
;;;;;;;;;;;;;;;;;;;;

;so we don't trash anything in the parent environment
;while we build up the functions...
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

(defmacro opt-uses (r _)
	(list 'find-rev r _))

(defun opt-find-1 (_ r x)
	(setq _ (some! _ 0 nil (lambda (e)
				(cond
					((match? e x) _)
					((opt-uses r e) -1))) (list inst_list))
		p (and _ (/= _ -1) _)))

(defun opt-find-2 (_ r x y)
	(setq _ (some! _ 0 nil (lambda (e)
				(cond
					((match? e x) (setq m 0) _)
					((match? e y) (setq m 1) _)
					((opt-uses r e) -1))) (list inst_list))
		p (and _ (/= _ -1) _)))

(defun opt-inst-list (inst_list)
	(defq i 0 p nil m nil)
	(while (< (setq i (inc i)) (length inst_list))
		(cond
			;variable loading and offset folding
			((find-rev (defq e (elem i inst_list) o (elem 0 e)) +opt_read_ops)
				(when (opt-find-2 i (defq r (elem 1 e)) `(vp-lea-i rsp _ ,r) `(vp-add-cr _ ,r))
					(cond
						((= m 0)
							(elem-set 1 e rsp)
							(elem-set 2 e `(+ ,(elem 2 (elem p inst_list)) ,(elem 2 e))))
						((= m 1)
							(elem-set 2 e `(+ ,(elem 1 (elem p inst_list)) ,(elem 2 e)))
							(setq i (dec i))))
					(elem-set p inst_list (lambda))))
			;variable writing and offset folding
			((find-rev o +opt_write_ops)
				(when (opt-find-2 i (defq r (elem 2 e)) `(vp-lea-i rsp _ ,r) `(vp-add-cr _ ,r))
					(cond
						((= m 0)
							(elem-set 2 e rsp)
							(elem-set 3 e `(+ ,(elem 2 (elem p inst_list)) ,(elem 3 e))))
						((= m 1)
							(elem-set 3 e `(+ ,(elem 1 (elem p inst_list)) ,(elem 3 e)))
							(setq i (dec i))))
					(elem-set p inst_list (lambda))))
			;strength reduction
			((and (eql o 'vp-mul-cr) (defq s (log2 (eval (elem 1 e)))))
				(elem-set 0 e 'vp-shl-cr)
				(elem-set 1 e s)
				(setq i (dec i)))
			;constant propagation
			((defq c (find-rev o +opt_rr_ops))
				(when (opt-find-1 i (defq r (elem 1 e)) `(vp-cpy-cr _ ,r))
					(elem-set 0 e (elem c +opt_cr_ops))
					(elem-set 1 e (elem 1 (elem p inst_list)))
					(elem-set p inst_list (lambda))
					(setq i (dec i))))
			;arithmetic reassignment and constant folding
			((and (defq c (find-rev o +opt_cr_ops))
				(opt-find-2 i (defq r (elem 2 e)) `(vp-cpy-cr _ ,r) (list o '_ r)))
					(cond
						((= m 0)
							(setq c (elem c +opt_fold_cpy_ops))
							(elem-set 0 e 'vp-cpy-cr))
						((= m 1)
							(setq c (elem c +opt_fold_cr_ops))))
					(elem-set 1 e (list c (elem 1 (elem p inst_list)) (elem 1 e)))
					(elem-set p inst_list (lambda)))
			;constant fusion
			((and (defq c (find-rev o +opt_cr_fuse_ops))
				(opt-find-2 i (defq r (elem 2 e)) `(vp-add-cr _ ,r) `(vp-sub-cr _ ,r)))
					(cond
						((= m 0)
							(setq c (elem c '(+ -))))
						((= m 1)
							(setq c (elem c '(- +)))))
					(elem-set 1 e (list c (elem 1 e) (elem 1 (elem p inst_list))))
					(elem-set p inst_list (lambda)))
			;compare constant forwarding
			((and (defq c (find-rev o +opt_sxx_rr_ops))
				(opt-find-1 i (defq r (elem 1 e)) `(vp-cpy-cr _ ,r))
				(<= -0x80000000 (defq v (eval (elem 1 (elem p inst_list)))) 0x7fffffff))
					(elem-set 0 e (elem c +opt_sxx_cr_ops))
					(elem-set 1 e v)
					(elem-set p inst_list (lambda)))
			)))

;export to the outside environment and wipe the temp environment.
(export (penv)
	opt-inst-list)
(env-pop)
