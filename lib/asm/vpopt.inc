;;;;;;;;;;;;;;
; VP optimizer
;;;;;;;;;;;;;;

;module
(env-push)

(defq
	+opt_emit_read_ops
		''(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir)
	+opt_emit_write_ops
		''(emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri)
	+opt_emit_write_ops_signed
		''(emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri)
	+opt_emit_read_ops_signed
		''(emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir)
	+opt_emit_read_ops_unsigned
		''(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub emit-cpy-ir)
	+opt_emit_call_ops
		''(emit-label emit-call emit-call-abi emit-call-i emit-call-r emit-call-p)
	+opt_emit_read_kill_ops
		''(emit-label emit-call emit-call-abi emit-call-i emit-call-r emit-call-p emit-alloc emit-free)
	+opt_emit_kill_ops
		''(emit-jmp emit-jmp-i emit-jmp-r emit-jmp-p emit-alloc emit-free emit-cmp-cr emit-cmp-rr)
	+opt_emit_two_out_ops
		''(emit-land-rr emit-lnot-rr emit-swp-rr emit-div-rrr emit-div-rrr-u)
	+opt_emit_multi_out_ops
		''(emit-pop))

(defmacro uses? (r op)
	`(find-rev ,r ,op))

(defmacro trashes-reg? (r op)
	`(cond
		((eql ,r (elem -2 ,op)))
		((find-rev (elem 0 ,op) ,'+opt_emit_two_out_ops) (eql ,r (elem -3 ,op)))
		((find-rev (elem 0 ,op) ,'+opt_emit_multi_out_ops) (uses? ,r ,op))))

(defmacro find-past (&rest _)
	`(and (defq _ (some! _ emit_start nil
			(lambda (op) (cond ~_))
		(list emit_list))) (> _ -1) _))

(defun find-past-rw (_ rop wop b i)
	(find-past
		((and (eql wop (elem 0 op))
				(eql b (elem 2 op))
				(= i (eval (elem 3 op))))
			(setq c 1) _)
		((and (eql rop (elem 0 op))
				(eql b (elem 1 op))
				(not (eql b (elem 3 op)))
				(= i (eval (elem 2 op))))
			(setq c 3) _)
		((trashes-reg? b op) -1)
		((find-rev (elem 0 op) +opt_emit_read_kill_ops) -1)))

(defun find-past-rw1 (_ rop1 rop2 wop b i)
	(find-past
		((and (eql wop (elem 0 op))
				(eql b (elem 2 op))
				(= i (eval (elem 3 op))))
			(setq c 1) _)
		((and (or (eql rop1 (elem 0 op)) (eql rop2 (elem 0 op)))
				(eql b (elem 1 op))
				(not (eql b (elem 3 op)))
				(= i (eval (elem 2 op))))
			(setq c 3) _)
		((trashes-reg? b op) -1)
		((find-rev (elem 0 op) +opt_emit_kill_ops) -1)
		((find-rev (elem 0 op) +opt_emit_call_ops) -1)))

(defun find-past-rr (_ rs rd)
	(find-past
		((match? op `(emit-cpy-rr ,rs ,rd)) _)
		((trashes-reg? rs op) -1)
		((trashes-reg? rd op) -1)
		((find-rev (elem 0 op) +opt_emit_call_ops) -1)))

(defun find-past-r (_ rd)
	(find-past
		((match? op `(emit-cpy-rr _ ,rd)) _)
		((uses? rd op) -1)
		((find-rev (elem 0 op) +opt_emit_kill_ops) -1)
		((find-rev (elem 0 op) +opt_emit_call_ops) -1)))

(defun opt-read-after-read-write ()
	(when (defq i (find-past-rw _
			(elem c +opt_emit_read_ops)
			(elem c +opt_emit_write_ops)
			(elem 1 op) (eval (elem 2 op))))
		(defq rs (elem c (elem i emit_list)))
		(unless (some! (inc i) _ nil (# (trashes-reg? rs %0)) (list emit_list))
			(elem-set _ emit_list
				(if (eql rs (elem 3 op))
					(setq op (lambda))
					(setq op `(emit-cpy-rr ,rs ,(elem 3 op))))))))

(defun opt-read-after-cpy ()
	(when (and (eql (elem 1 op) (elem 3 op))
			(defq i (find-past-r _ (elem 1 op))))
		(defq rs (elem 1 (elem i emit_list)))
		(unless (some! (inc i) _ nil (# (trashes-reg? rs %0)) (list emit_list))
			(elem-set 1 op rs)
			(elem-set i emit_list op)
			(elem-set _ emit_list (setq op (lambda))))))

(defun opt-write-after-write ()
	(when (defq w (find-past-rw1 _
			(elem c +opt_emit_read_ops_signed)
			(elem c +opt_emit_read_ops_unsigned)
			(elem c +opt_emit_write_ops_signed)
			(elem 2 op) (eval (elem 3 op))))
		(when (= c 1)
			(elem-set w emit_list (lambda)))))

(defun opt-redundant-cpy ()
	(if (or (eql (elem 1 op) (elem 2 op))
			(find-past-rr _ (elem 2 op) (elem 1 op)))
		(elem-set _ emit_list (setq op (lambda)))))

(defun opt-emit-list (emit_list emit_start emit_end)
	(each! emit_start emit_end (lambda (op)
			(cond
				((defq c (find-rev (elem 0 op) +opt_emit_read_ops))
					(opt-read-after-read-write)
					(if (find-rev (elem 0 op) +opt_emit_read_ops)
						(opt-read-after-cpy)))
				((defq c (find-rev (elem 0 op) +opt_emit_write_ops_signed))
					(opt-write-after-write))
				((eql (elem 0 op) 'emit-cpy-rr)
					(opt-redundant-cpy))))
		(list emit_list))
	(each! emit_start emit_end (lambda (op)
			(if (defq c (find-rev (elem 0 op) +opt_emit_read_ops))
				(opt-read-after-read-write)))
		(list emit_list)))

;module
(export-symbols
	opt-emit-list)
(env-pop)
