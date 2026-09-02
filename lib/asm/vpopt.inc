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
		''(emit-cpy-ir emit-cpy-ir-ui emit-cpy-ir-i emit-cpy-ir-ub emit-cpy-ir-us emit-cpy-ir-b emit-cpy-ir-s)
	+opt_write_ops
		''(emit-cpy-ri emit-cpy-ri-i emit-cpy-ri-i emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri-s)
	+opt_write_ops_signed
		''(emit-cpy-ri emit-cpy-ri-i emit-cpy-ri-b emit-cpy-ri-s)
	+opt_read_ops_signed
		''(emit-cpy-ir emit-cpy-ir-i emit-cpy-ir-b emit-cpy-ir-s)
	+opt_read_ops_unsigned
		''(emit-cpy-ir emit-cpy-ir-ui emit-cpy-ir-ub emit-cpy-ir-us)
	+opt_branch_ops
		''(emit-beq-cr emit-bne-cr emit-bge-cr emit-ble-cr emit-blt-cr emit-bgt-cr))

(defq
	+opt_read_ops_map
		(reduce (# (pinsert %0 %1 (find %1 +opt_read_ops)))
			+vp_emit_ops (pmap))
	+opt_write_ops_signed_map
		(reduce (# (pinsert %0 %1 (find %1 +opt_write_ops_signed)))
			+vp_emit_ops (pmap))
	+opt_branch_ops_map
		(reduce (# (pinsert %0 %1 (find %1 +opt_branch_ops)))
			+vp_emit_ops (pmap))
	+opt_read_kill_ops
		(reduce (# (pinsert %0 %1
			(if (find %1 '(emit-label emit-call-p emit-free emit-alloc
					emit-call-i emit-call-abi emit-call emit-call-r)) -1)))
			+vp_emit_ops (pmap))
	+opt_write_kill_ops
		(reduce (# (pinsert %0 %1
			(if (find %1 '(emit-label emit-call-p emit-free emit-alloc
					emit-call-i emit-call-abi emit-call emit-call-r
					emit-jmp emit-beq-cr emit-bne-cr emit-bne-rr emit-beq-rr emit-bge-rr
					emit-ble-rr emit-jmp-p emit-bge-cr emit-ble-cr emit-blt-cr emit-blt-rr
					emit-bgt-rr emit-bgt-cr emit-jmp-r emit-jmp-i emit-beq-ff emit-bne-ff
					emit-blt-ff emit-bgt-ff emit-ble-ff emit-bge-ff)) -1)))
			+vp_emit_ops (pmap))
	+opt_out_idx_map
		(reduce (# (pinsert %0 %1
			(cond
				((find %1 '(emit-pop emit-swp-rr emit-land-rr emit-lnot-rr emit-div-rrr emit-div-rrr-u)) :multi)
				((find %1 '(emit-cpy-cr emit-add-cr emit-sub-cr emit-mul-cr emit-and-cr emit-or-cr emit-xor-cr
						emit-cpy-rr emit-add-rr emit-sub-rr emit-mul-rr emit-and-rr emit-or-rr emit-xor-rr
						emit-shl-cr emit-shr-cr emit-asr-cr emit-shl-rr emit-shr-rr emit-asr-rr emit-ext-rr
						emit-seq-cr emit-sne-cr emit-slt-cr emit-sle-cr emit-sgt-cr emit-sge-cr
						emit-seq-rr emit-sne-rr emit-slt-rr emit-sle-rr emit-sgt-rr emit-sge-rr
						emit-min-cr emit-max-cr emit-min-rr emit-max-rr emit-abs-rr
						emit-cpy-ir emit-cpy-ir-b emit-cpy-ir-s emit-cpy-ir-i emit-cpy-ir-ub emit-cpy-ir-us emit-cpy-ir-ui emit-lea-i
						emit-cpy-dr emit-cpy-dr-b emit-cpy-dr-s emit-cpy-dr-i emit-cpy-dr-ub emit-cpy-dr-us emit-cpy-dr-ui emit-lea-d
						emit-cpy-pr emit-lea-p
						emit-add-ff emit-sub-ff emit-mul-ff emit-div-ff emit-min-ff emit-max-ff
						emit-sqrt-ff emit-abs-ff emit-neg-ff emit-cpy-ff
						emit-cvt-rf emit-cvt-fr emit-cpy-rf emit-cpy-fr
						emit-cpy-if emit-cpy-df)) -2))))
			+vp_emit_ops (pmap)))

(defmacro uses? (r inst)
	(static-qq (find ,r ,inst 1)))

(defmacro trashes-reg? (r inst)
	(static-qqp (cond
		((eql (defq idx (pfind +opt_out_idx_map (first ,inst))) :multi) (uses? ,r ,inst))
		(idx (eql ,r (elem-get ,inst idx))))))

(defmacro find-past (&rest body)
	(static-qq (and (defq _ (some! (,'lambda (inst) (cond ~body))
		(list emit_list) :nil _ emit_start)) (> _ -1) _)))

(defun find-past-rw (_ rop wop b i)
	(find-past
		((and (eql wop (first inst)) (eql b (third inst)) (= i (last inst)))
			(setq c 1) (!))
		((and (eql rop (first inst)) (eql b (second inst)) (nql b (last inst)) (= i (third inst)))
			(setq c 3) (!))
		((trashes-reg? b inst) -1)
		((pfind +opt_read_kill_ops (first inst)))))

(defun find-past-w (_ rop1 rop2 wop b i)
	(find-past
		((and (eql wop (first inst)) (eql b (third inst)) (= i (last inst))) (!))
		((and (or (eql rop1 (first inst)) (eql rop2 (first inst)))
			(eql b (second inst)) (nql b (last inst)) (= i (third inst))) -1)
		((trashes-reg? b inst) -1)
		((pfind +opt_write_kill_ops (first inst)))))

(defun find-past-rr (_ rs rd)
	(find-past
		((lmatch? inst `(emit-cpy-rr ,rs ,rd)) (!))
		((trashes-reg? rs inst) -1)
		((trashes-reg? rd inst) -1)
		((pfind +opt_read_kill_ops (first inst)))))

(defun find-past-r (_ rd)
	(find-past
		((lmatch? inst `(emit-cpy-rr _ ,rd)) (!))
		((uses? rd inst) -1)
		((pfind +opt_read_kill_ops (first inst)))))

(defun find-past-cr (_ rd)
	(find-past
		((lmatch? inst `(emit-cpy-cr _ ,rd)) (!))
		((trashes-reg? rd inst) -1)
		((pfind +opt_read_kill_ops (first inst)))))

(defun opt-read-after-read-write ()
	(when (and (num? (third inst))
			(defq i (find-past-rw (!)
			(elem-get +opt_read_ops c)
			(elem-get +opt_write_ops c)
			(second inst) (third inst))))
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
		(if (eval (elem-get (static-q ((/= i j)(= i j)(> i j)(< i j)(<= i j)(>= i j))) c))
			(elem-set emit_list (!) (setq inst +opt_nop)))))

(defun opt-tail-call ()
	(when (defq i (find (first (defq prev_inst (elem-get emit_list (dec (!))))) +opt_call_ops))
		(elem-set prev_inst 0 (elem-get +opt_jump_ops i))
		(elem-set emit_list (!) (setq inst +opt_nop))))

(defun opt-emit-list (emit_list emit_start emit_end)
	(each! (lambda (inst)
		(cond
			((defq c (pfind +opt_read_ops_map (defq op (first inst))))
				(opt-read-after-read-write)
				(if (pfind +opt_read_ops_map (first inst))
					(opt-read-after-cpy)))
			((defq c (pfind +opt_write_ops_signed_map op))
				(opt-write-after-write))
			((defq c (pfind +opt_branch_ops_map op))
				(opt-redundant-branch))
			((eql op 'emit-cpy-rr)
				(opt-redundant-cpy))
			((eql op 'emit-ret)
				(opt-tail-call))))
		(list emit_list) emit_start emit_end)
	(each! (lambda (inst)
			(if (defq c (pfind +opt_read_ops_map (first inst)))
				(opt-read-after-read-write)))
		(list emit_list) emit_start emit_end))

;module
(export-symbols '(opt-emit-list))
(env-pop)