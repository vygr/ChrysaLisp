;;;;;;;;;;;;;;
; VP optimizer
;;;;;;;;;;;;;;

;module
(env-push)

(defq
	+opt_nop
		'(lambda)
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
		''(emit-jmp emit-jmp-i emit-jmp-r emit-jmp-p emit-alloc emit-free
			emit-beq-cr emit-bne-cr emit-bge-cr emit-blt-cr emit-ble-cr emit-bgt-cr
			emit-beq-rr emit-bne-rr emit-bge-rr emit-blt-rr emit-ble-rr emit-bgt-rr)
	+opt_emit_two_out_ops
		''(emit-land-rr emit-lnot-rr emit-swp-rr emit-div-rrr emit-div-rrr-u)
	+opt_emit_multi_out_ops
		''(emit-pop))

(defmacro uses? (r op)
	`(find-rev ,r ,op))

(defmacro trashes-reg? (r op)
	`(cond
		((eql ,r (last ,op)))
		((find (first ,op) ,'+opt_emit_two_out_ops) (eql ,r (elem-get ,op -3)))
		((find-rev (first ,op) ,'+opt_emit_multi_out_ops) (uses? ,r ,op))))

(defmacro find-past (&rest body)
	`(and (defq _ (some! (lambda (op) (cond ~body))
		(list emit_list) :nil _ emit_start)) (> _ -1) _))

(defun find-past-rw (_ rop wop b i)
	(find-past
		((and (eql wop (first op))
				(eql b (third op))
				(= i (eval (elem-get op 3))))
			(setq c 1) (!))
		((and (eql rop (first op))
				(eql b (second op))
				(not (eql b (elem-get op 3)))
				(= i (eval (third op))))
			(setq c 3) (!))
		((trashes-reg? b op) -1)
		((find-rev (first op) +opt_emit_read_kill_ops) -1)))

(defun find-past-rw1 (_ rop1 rop2 wop b i)
	(find-past
		((and (eql wop (first op))
				(eql b (third op))
				(= i (eval (elem-get op 3))))
			(setq c 1) (!))
		((and (or (eql rop1 (first op)) (eql rop2 (first op)))
				(eql b (second op))
				(not (eql b (elem-get op 3)))
				(= i (eval (third op))))
			(setq c 3) (!))
		((trashes-reg? b op) -1)
		((find-rev (first op) +opt_emit_kill_ops) -1)
		((find-rev (first op) +opt_emit_call_ops) -1)))

(defun find-past-rr (_ rs rd)
	(find-past
		((lmatch? op `(emit-cpy-rr ,rs ,rd)) (!))
		((trashes-reg? rs op) -1)
		((trashes-reg? rd op) -1)
		((find-rev (first op) +opt_emit_call_ops) -1)))

(defun find-past-r (_ rd)
	(find-past
		((lmatch? op `(emit-cpy-rr _ ,rd)) (!))
		((uses? rd op) -1)
		((find-rev (first op) +opt_emit_kill_ops) -1)
		((find-rev (first op) +opt_emit_call_ops) -1)))

(defun opt-read-after-read-write ()
	(when (defq i (find-past-rw (!)
			(elem-get +opt_emit_read_ops c)
			(elem-get +opt_emit_write_ops c)
			(second op) (eval (third op))))
		(defq rs (elem-get (elem-get emit_list i) c))
		(unless (some! (# (trashes-reg? rs %0)) (list emit_list) :nil (inc i) (!))
			(elem-set emit_list (!)
				(if (eql rs (elem-get op 3))
					(setq op +opt_nop)
					(setq op `(emit-cpy-rr ,rs ,(elem-get op 3))))))))

(defun opt-read-after-cpy ()
	(when (and (eql (second op) (elem-get op 3))
			(defq i (find-past-r (!) (second op))))
		(defq rs (second (elem-get emit_list i)))
		(unless (some! (# (trashes-reg? rs %0)) (list emit_list) :nil (inc i) (!))
			(elem-set op 1 rs)
			(elem-set emit_list i op)
			(elem-set emit_list (!) (setq op +opt_nop)))))

(defun opt-write-after-write ()
	(when (defq w (find-past-rw1 (!)
			(elem-get +opt_emit_read_ops_signed c)
			(elem-get +opt_emit_read_ops_unsigned c)
			(elem-get +opt_emit_write_ops_signed c)
			(third op) (eval (elem-get op 3))))
		(when (= c 1)
			(elem-set emit_list w +opt_nop))))

(defun opt-redundant-cpy ()
	(if (or (eql (second op) (third op))
			(find-past-rr (!) (third op) (second op)))
		(elem-set emit_list (!) (setq op +opt_nop))))

(defun opt-emit-list (emit_list emit_start emit_end)
	(each! (lambda (op)
		(cond
			((defq c (find-rev (first op) +opt_emit_read_ops))
				(opt-read-after-read-write)
				(if (find-rev (first op) +opt_emit_read_ops)
					(opt-read-after-cpy)))
			((defq c (find-rev (first op) +opt_emit_write_ops_signed))
				(opt-write-after-write))
			((eql (first op) 'emit-cpy-rr)
				(opt-redundant-cpy))))
		(list emit_list) emit_start emit_end)
	(each! (lambda (op)
		(if (defq c (find-rev (first op) +opt_emit_read_ops))
			(opt-read-after-read-write)))
		(list emit_list) emit_start emit_end))

;module
(export-symbols '(opt-emit-list))
(env-pop)
