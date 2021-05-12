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
	(list 'find-rev r op))

(defmacro trashes-reg? (r op)
	`(cond
		((eql ,r (elem -2 ,op)))
		((find-rev (elem 0 ,op) ,'+opt_emit_two_out_ops) (eql ,r (elem -3 ,op)))
		((find-rev (elem 0 ,op) ,'+opt_emit_multi_out_ops) (uses? ,r ,op))))

(defun find-past-rw (_ rop wop b i)
	(and (defq _ (some! _ 1 nil
		(lambda (op)
			(cond
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
		(list emit_list))) (> _ -1) _))

(defun find-past-rw1 (_ rop1 rop2 wop b i)
	(and (defq _ (some! _ 1 nil
		(lambda (op)
			(cond
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
		(list emit_list))) (> _ -1) _))

(defun find-past-rr (_ rs rd)
	(and (defq _ (some! _ 1 nil
		(lambda (op)
			(cond
				((and (eql (elem 0 op) 'emit-cpy-rr)
						(eql rs (elem 1 op))
						(eql rd (elem 2 op)))
					_)
				((trashes-reg? rs op) -1)
				((trashes-reg? rd op) -1)
				((find-rev (elem 0 op) +opt_emit_call_ops) -1)))
		(list emit_list))) (> _ -1) _))

;; (defun find-future-r (_ rs b)
;; 	(and (defq _ (some! (inc _) -1 nil
;; 		(lambda (op)
;; 			(cond
;; 				((and (find-rev (elem 0 op) +opt_emit_read_ops)
;; 						(eql b (elem 1 op))
;; 						(eql b (elem 3 op)))
;; 					_)
;; 				((uses? b op) -1)
;; 				((trashes-reg? rs op) -1)
;; 				((find-rev (elem 0 op) +opt_emit_kill_ops) -1)
;; 				((find-rev (elem 0 op) +opt_emit_call_ops) -1)))
;; 		(list emit_list))) (> _ -1) _))

(defun opt-emit-list (emit_list)
	(each! 1 -1 (lambda (op)
		(cond
			;read after read/write
			((defq c (find-rev (elem 0 op) +opt_emit_read_ops))
				(when (defq w (find-past-rw _
						(elem c +opt_emit_read_ops)
						(elem c +opt_emit_write_ops)
						(elem 1 op) (eval (elem 2 op))))
					(defq r (elem c (elem w emit_list)))
					(unless (some! (inc w) _ nil (lambda (_)
							(trashes-reg? r _)) (list emit_list))
						(elem-set _ emit_list
							(if (eql r (elem 3 op))
								(lambda)
								`(emit-cpy-rr ,r ,(elem 3 op)))))))
			;write after write
			((defq c (find-rev (elem 0 op) +opt_emit_write_ops_signed))
				(when (defq w (find-past-rw1 _
						(elem c +opt_emit_read_ops_signed)
						(elem c +opt_emit_read_ops_unsigned)
						(elem c +opt_emit_write_ops_signed)
						(elem 2 op) (eval (elem 3 op))))
					(when (= c 1)
						(elem-set w emit_list (lambda)))))
			;redundant copies
			((eql (elem 0 op) 'emit-cpy-rr)
				(cond
					((eql (elem 1 op) (elem 2 op))
						(elem-set _ emit_list (lambda)))
					((find-past-rr _ (elem 2 op) (elem 1 op))
						(elem-set _ emit_list (lambda)))
					;; ((defq w (find-future-r _ (elem 1 op) (elem 2 op)))
					;; 	(elem-set 1 (elem w emit_list) (elem 1 op))
					;; 	(elem-set _ emit_list (elem w emit_list))
					;; 	(elem-set w emit_list (lambda)))
					))))
	(list emit_list)))

;module
(export (penv)
	opt-emit-list)
(env-pop)
