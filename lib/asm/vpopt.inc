;;;;;;;;;;;;;;
; VP optimizer
;;;;;;;;;;;;;;

;module
(env-push)

(defq
	+opt_nop
		''(emit-vp-nop)
	+opt_call_ops
		''(emit-call-p emit-call-i emit-call emit-call-r)
	+opt_jump_ops
		''(emit-jmp-p emit-jmp-i emit-jmp emit-jmp-r)
	+opt_read_ops
		''(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir)
	+opt_write_ops
		''(emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri)
	+opt_write_ops_signed
		''(emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri)
	+opt_read_ops_signed
		''(emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir)
	+opt_read_ops_unsigned
		''(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub emit-cpy-ir)
	+opt_read_kill_ops
		''(emit-label emit-call-p emit-alloc emit-free emit-call-i emit-call-abi emit-call emit-call-r)
	+opt_write_kill_ops
		''(emit-jmp emit-jmp-i emit-jmp-r emit-jmp-p
			emit-beq-cr emit-bne-cr emit-bge-cr emit-blt-cr emit-ble-cr emit-bgt-cr
			emit-beq-rr emit-bne-rr emit-bge-rr emit-blt-rr emit-ble-rr emit-bgt-rr)
	+opt_branch_ops
		''(emit-beq-cr emit-bne-cr emit-bge-cr emit-blt-cr emit-ble-cr emit-bgt-cr)
	+opt_two_out_ops
		''(emit-land-rr emit-lnot-rr emit-swp-rr emit-div-rrr emit-div-rrr-u)
	+opt_multi_out_ops
		''(emit-pop))

(defmacro uses? (r inst)
	(static-qq (find ,r ,inst 1)))

(defmacro trashes-reg? (r inst)
	(static-qqp (cond
		((eql ,r (last ,inst)))
		((find (first ,inst) ,'+opt_two_out_ops) (eql ,r (elem-get ,inst -3)))
		((find (first ,inst) ,'+opt_multi_out_ops) (uses? ,r ,inst)))))

(defmacro find-past (&rest body)
	(static-qq (and (defq _ (some! (,'lambda (inst) (cond ~body))
		(list emit_list) :nil _ emit_start)) (> _ -1) _)))

(defun find-past-rw (_ rop wop b i)
	(find-past
		((and (eql wop (first inst)) (eql b (third inst)) (= i (last inst)))
			(setq c 1) (!))
		((and (eql rop (first inst)) (eql b (second inst)) (not (eql b (last inst))) (= i (third inst)))
			(setq c 3) (!))
		((trashes-reg? b inst) -1)
		((find (first inst) +opt_read_kill_ops) -1)))

(defun find-past-w (_ rop1 rop2 wop b i)
	(find-past
		((and (eql wop (first inst)) (eql b (third inst)) (= i (last inst))) (!))
		((and (or (eql rop1 (first inst)) (eql rop2 (first inst)))
			(eql b (second inst)) (not (eql b (last inst))) (= i (third inst))) -1)
		((trashes-reg? b inst) -1)
		((find (first inst) +opt_read_kill_ops) -1)
		((find (first inst) +opt_write_kill_ops) -1)))

(defun find-past-rr (_ rs rd)
	(find-past
		((lmatch? inst `(emit-cpy-rr ,rs ,rd)) (!))
		((trashes-reg? rs inst) -1)
		((trashes-reg? rd inst) -1)
		((find (first inst) +opt_read_kill_ops) -1)))

(defun find-past-r (_ rd)
	(find-past
		((lmatch? inst `(emit-cpy-rr _ ,rd)) (!))
		((uses? rd inst) -1)
		((find (first inst) +opt_read_kill_ops) -1)))

(defun find-past-cr (_ rd)
	(find-past
		((lmatch? inst `(emit-cpy-cr _ ,rd)) (!))
		((trashes-reg? rd inst) -1)
		((find (first inst) +opt_read_kill_ops) -1)))

(defun opt-read-after-read-write ()
	(when (defq i (find-past-rw (!)
			(elem-get +opt_read_ops c)
			(elem-get +opt_write_ops c)
			(second inst) (third inst)))
		(defq rs (elem-get (elem-get emit_list i) c))
		(unless (some! (# (trashes-reg? rs %0)) (list emit_list) :nil (inc i) (!))
			(elem-set emit_list (!)
				(if (eql rs (last inst))
					(setq inst +opt_nop)
					(setq inst (list 'emit-cpy-rr rs (last inst))))))))

(defun opt-read-after-cpy ()
	(when (and (eql (second inst) (last inst))
			(defq i (find-past-r (!) (second inst))))
		(defq rs (second (elem-get emit_list i)))
		(unless (some! (# (trashes-reg? rs %0)) (list emit_list) :nil (inc i) (!))
			(elem-set inst 1 rs)
			(elem-set emit_list i inst)
			(elem-set emit_list (!) (setq inst +opt_nop)))))

(defun opt-write-after-write ()
	(if (defq i (find-past-w (!)
			(elem-get +opt_read_ops_signed c)
			(elem-get +opt_read_ops_unsigned c)
			(elem-get +opt_write_ops_signed c)
			(third inst) (last inst)))
		(elem-set emit_list i +opt_nop)))

(defun opt-redundant-cpy ()
	(if (or (eql (second inst) (third inst))
			(find-past-rr (!) (third inst) (second inst)))
		(elem-set emit_list (!) (setq inst +opt_nop))))

(defun opt-redundant-branch ()
	(when (defq i (find-past-cr (!) (third inst)))
		(defq j (second (elem-get emit_list i)) i (second inst))
		(if (eval (elem-get (static-q ((/= i j)(= i j)(> i j)(<= i j)(< i j)(>= i j))) c))
			(elem-set emit_list (!) (setq inst +opt_nop)))))

(defun opt-tail-call ()
	(when (defq i (find (first (defq prev_inst (elem-get emit_list (dec (!))))) +opt_call_ops))
		(elem-set prev_inst 0 (elem-get +opt_jump_ops i))
		(elem-set emit_list (!) (setq inst +opt_nop))))

(defun opt-emit-list (emit_list emit_start emit_end)
	(each! (lambda (inst)
		(cond
			((defq c (find (defq op (first inst)) +opt_read_ops)) (opt-read-after-read-write)
				(if (find (first inst) +opt_read_ops) (opt-read-after-cpy)))
			((defq c (find op +opt_write_ops_signed)) (opt-write-after-write))
			((defq c (find op +opt_branch_ops)) (opt-redundant-branch))
			((eql op 'emit-cpy-rr) (opt-redundant-cpy))
			((eql op 'emit-ret) (opt-tail-call))))
		(list emit_list) emit_start emit_end)
	(each! (lambda (inst)
			(if (defq c (find (first inst) +opt_read_ops)) (opt-read-after-read-write)))
		(list emit_list) emit_start emit_end))

;module
(export-symbols '(opt-emit-list))
(env-pop)
