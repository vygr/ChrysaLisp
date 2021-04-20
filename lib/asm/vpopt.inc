;;;;;;;;;;;;;;
; VP optimizer
;;;;;;;;;;;;;;

;so we don't trash anything in the parent environment
;while we build up the functions...
(env-push)

(defq
	+opt_emit_read_ops
		''(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir)
	+opt_emit_write_ops
		''(emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri)
	+opt_emit_write_ops1
		''(emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri)
	+opt_emit_read_ops1
		''(emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir)
	+opt_emit_read_ops2
		''(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub emit-cpy-ir)
	+opt_emit_call_ops
		''(emit-label emit-call emit-call-abi emit-call-i emit-call-r emit-call-p)
	+opt_emit_read_kills_ops
		''(emit-label emit-call emit-call-abi emit-call-i emit-call-r emit-call-p emit-alloc emit-free)
	+opt_emit_kills_ops
		''(emit-jmp emit-jmp-i emit-jmp-r emit-jmp-p emit-alloc emit-free emit-cmp-cr emit-cmp-rr)
	+opt_emit_two_out_ops
		''(emit-land-rr emit-lnot-rr emit-swp-rr emit-div-rrr emit-div-rrr-u)
	+opt_emit_multi_out_ops
		''(emit-pop))

(defmacro opt-trashes? (r e)
	`(cond
		((eql ,r (elem -2 ,e)) t)
		((find-rev (elem 0 ,e) ,'+opt_emit_two_out_ops) (eql ,r (elem -3 ,e)))
		((find-rev (elem 0 ,e) ,'+opt_emit_multi_out_ops) (find-rev ,r ,e))))

(defun opt-emit-trashes? (r e)
	(opt-trashes? r e))

(defun opt-emit-trashes-with-calls? (r e)
	(cond
		((find-rev (elem 0 e) +opt_emit_call_ops) t)
		(t (opt-trashes? r e))))

(defun opt-emit-trashes-reads? (r e)
	(cond
		((find-rev (elem 0 e) +opt_emit_read_kills_ops) t)
		(t (opt-trashes? r e))))

(defun opt-emit-trashes-with-kills? (r e)
	(cond
		((find-rev (elem 0 e) +opt_emit_kills_ops) t)
		((find-rev (elem 0 e) +opt_emit_call_ops) t)
		(t (opt-trashes? r e))))

(defun opt-emit-find-rw (k _ r w b i)
	(defq _ (some! _ 0 nil (lambda (e)
		(cond
			((and (eql w (elem 0 e))
				(eql b (elem 2 e))
				(= i (eval (elem 3 e))))
				(setq c 1) _)
			((and (eql r (elem 0 e))
				(eql b (elem 1 e))
				(not (eql b (elem 3 e)))
				(= i (eval (elem 2 e))))
				(setq c 3) _)
			((k b e) -1))) (list *emit_buffer*)))
	(and _ (/= _ -1) _))

(defun opt-emit-find-rw1 (k _ ra rb w b i)
	(defq _ (some! _ 0 nil (lambda (e)
		(cond
			((and (eql w (elem 0 e))
				(eql b (elem 2 e))
				(= i (eval (elem 3 e))))
				(setq c 1) _)
			((and (or (eql ra (elem 0 e)) (eql rb (elem 0 e)))
				(eql b (elem 1 e))
				(not (eql b (elem 3 e)))
				(= i (eval (elem 2 e))))
				(setq c 3) _)
			((k b e) -1))) (list *emit_buffer*)))
	(and _ (/= _ -1) _))

(defun opt-emit-find-rr (k _ rs rd)
	(defq _ (some! _ 0 nil (lambda (e)
		(cond
			((match? e `(emit-cpy-rr ,rs ,rd)) _)
			((opt-trashes? rs e) -1)
			((k rd e) -1))) (list *emit_buffer*)))
	(and _ (/= _ -1) _))

(defun opt-emit-buffer ()
	(each! 2 -1 (lambda (e)
		(cond
			;read after read/write
			((defq c (find-rev (defq o (elem 0 e)) +opt_emit_read_ops))
				(when (defq w (opt-emit-find-rw (const opt-emit-trashes-reads?) _
								(elem c +opt_emit_read_ops)
								(elem c +opt_emit_write_ops)
								(elem 1 e) (eval (elem 2 e))))
					(defq r (elem c (elem w *emit_buffer*)))
					(when (not (some! (inc w) _ nil (lambda (_)
							(opt-emit-trashes? r _)) (list *emit_buffer*)))
						(elem-set _ *emit_buffer*
							(if (eql r (elem 3 e))
								(lambda)
								`(emit-cpy-rr ,r ,(elem 3 e)))))))
			;write after write
			((defq c (find-rev o +opt_emit_write_ops1))
				(when (defq w (opt-emit-find-rw1 (const opt-emit-trashes-with-kills?) _
								(elem c +opt_emit_read_ops1)
								(elem c +opt_emit_read_ops2)
								(elem c +opt_emit_write_ops1)
								(elem 2 e) (eval (elem 3 e))))
					(when (= c 1)
						(elem-set w *emit_buffer* (lambda)))))
			;redundant copies
			((eql o 'emit-cpy-rr)
				(when (opt-emit-find-rr (const opt-emit-trashes-with-calls?) _ (elem 2 e) (elem 1 e))
					(elem-set _ *emit_buffer* (lambda))))
			))
		(list *emit_buffer*)))

;export symbols to the outside environment and wipe the temp environment.
(export (penv)
	opt-emit-buffer)
(env-pop)
