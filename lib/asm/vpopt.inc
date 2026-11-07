;;;;;;;;;;;;;;
; VP optimizer
;;;;;;;;;;;;;;

;module
(env-push)

(defq
	+opt_nop
		''(emit-vp-nop)
	+opt_call_ops
		''(emit-call-p emit-call-rc emit-call emit-call-r)
	+opt_jump_ops
		''(emit-jmp-p emit-jmp-rc emit-jmp emit-jmp-r)
	+opt_read_ops
		''(emit-load-rcr emit-load-rcr-ui emit-load-rcr-i emit-load-rcr-ub emit-load-rcr-us emit-load-rcr-b emit-load-rcr-s)
	+opt_write_ops
		''(emit-store-rrc emit-store-rrc-i emit-store-rrc-i emit-store-rrc-b emit-store-rrc-s emit-store-rrc-b emit-store-rrc-s)
	+opt_write_ops_signed
		''(emit-store-rrc emit-store-rrc-i emit-store-rrc-b emit-store-rrc-s)
	;those of a float, with an offset where an int op has it
	+opt_read_ops_float
		''(emit-load-rcd emit-load-rcd-i)
	+opt_write_ops_float
		''(emit-store-drc emit-store-drc-i)
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
	;a push or a pop moves :rsp, as an alloc or a free does, so what was at
	;an offset from it before is not what is there after
	+opt_read_kill_ops
		(reduce (# (pinsert %0 %1
			(if (find %1 '(emit-label emit-call-p emit-free emit-alloc emit-push emit-pop emit-save-state emit-load-state emit-entry-abi
					emit-call-rc emit-call-abi emit-call emit-call-r)) -1)))
			+vp_emit_ops (pmap))
	+opt_write_kill_ops
		(reduce (# (pinsert %0 %1
			(if (find %1 '(emit-label emit-call-p emit-free emit-alloc emit-push emit-pop emit-save-state emit-load-state emit-entry-abi
					emit-call-rc emit-call-abi emit-call emit-call-r
					emit-jmp emit-beq-cr emit-bne-cr emit-bne-rr emit-beq-rr emit-bge-rr
					emit-ble-rr emit-jmp-p emit-bge-cr emit-ble-cr emit-blt-cr emit-blt-rr
					emit-bgt-rr emit-bgt-cr emit-jmp-r emit-jmp-rc emit-beq-ff emit-bne-ff
					emit-blt-ff emit-bgt-ff emit-ble-ff emit-bge-ff)) -1)))
			+vp_emit_ops (pmap))
	+opt_out_idx_map
		(reduce (# (pinsert %0 %1
			(cond
				((find %1 '(emit-pop emit-swp-rr emit-land-rr)) :multi)
				((find %1 '(emit-cpy-cr emit-add-cr emit-sub-cr emit-mul-cr emit-and-cr emit-or-cr emit-xor-cr
						emit-cpy-rr emit-add-rr emit-sub-rr emit-mul-rr emit-and-rr emit-or-rr emit-xor-rr
						emit-div-rr emit-div-rr-u emit-rem-rr emit-rem-rr-u
						emit-shl-cr emit-shr-cr emit-asr-cr emit-shl-rr emit-shr-rr emit-asr-rr emit-ext-rr
						emit-seq-cr emit-sne-cr emit-slt-cr emit-sle-cr emit-sgt-cr emit-sge-cr
						emit-seq-rr emit-sne-rr emit-slt-rr emit-sle-rr emit-sgt-rr emit-sge-rr
						emit-min-cr emit-max-cr emit-min-rr emit-max-rr emit-abs-rr
						emit-load-rcr emit-load-rcr-b emit-load-rcr-s emit-load-rcr-i emit-load-rcr-ub emit-load-rcr-us emit-load-rcr-ui emit-lea-rcr
						emit-load-rrr emit-load-rrr-b emit-load-rrr-s emit-load-rrr-i emit-load-rrr-ub emit-load-rrr-us emit-load-rrr-ui emit-lea-rrr
						emit-load-pr emit-lea-pr
						emit-add-ff emit-sub-ff emit-mul-ff emit-div-ff emit-min-ff emit-max-ff
						emit-sqrt-ff emit-abs-ff emit-neg-ff emit-cpy-ff
						emit-cvt-rf emit-cvt-fr emit-cpy-rf emit-cpy-fr
						emit-load-rcd emit-load-rrd emit-load-rcd-i emit-load-rrd-i emit-lnot-rr)) -2))))
			+vp_emit_ops (pmap)))

;one lookup an instruction. For the sweep, the pass an op is for, and its
;index in that pass's op lists. For a scan back, what an op does, the
;index of the register it writes, and if it ends a read or a write scan.
;Each is made over +vp_emit_ops, in its order, as are all the maps of VP
;ops, so a symbol's cached slot is right for every one of them.
(defq
	+opt_op_map
		(reduce (# (pinsert %0 %1
			(cond
				((defq c (pfind +opt_read_ops_map %1)) (list :read c))
				((defq c (pfind +opt_write_ops_signed_map %1)) (list :write c))
				((defq c (pfind +opt_branch_ops_map %1)) (list :branch c))
				((eql %1 'emit-cpy-rr) (list :cpy 0))
				((eql %1 'emit-ret) (list :ret 0)))))
			+vp_emit_ops (pmap))
	+opt_info_map
		(reduce (# (pinsert %0 %1 (list (pfind +opt_out_idx_map %1)
				(pfind +opt_read_kill_ops %1) (pfind +opt_write_kill_ops %1))))
			+vp_emit_ops (pmap)))

(defmacro uses? (r inst)
	(static-qq (find ,r ,inst 1)))

(defmacro trashes-reg? (r inst)
	(static-qqp (cond
		((eql (defq idx (pfind +opt_out_idx_map (first ,inst))) :multi) (uses? ,r ,inst))
		(idx (eql ,r (elem-get ,inst idx))))))

(defmacro trashes? (r inst info)
	;as trashes-reg?, with the info of the op to hand
	(static-qqp (cond
		((eql (defq idx (first ,info)) :multi) (uses? ,r ,inst))
		(idx (eql ,r (elem-get ,inst idx))))))

(defmacro find-past (&rest body)
	(static-qq (and (defq _ (some! (,'lambda (inst) (cond ~body))
		(list emit_list) :nil _ emit_start)) (> _ -1) _)))

(defun find-past-rw (_ rop wop b i)
	;a store of a whole word there is what a load of it gives; a store of
	;fewer bytes, or another, ends the search: the load extends what is
	;stored, and before the store there was something else
	(find-past
		((and (or (find (first inst) +opt_write_ops) (find (first inst) +opt_write_ops_float))
				(eql b (third inst)) (= i (last inst)))
			(if (and (eql wop (first inst)) (eql wop 'emit-store-rrc)) (progn (setq c 1) (!)) -1))
		((and (eql rop (first inst)) (eql b (second inst)) (nql b (last inst)) (= i (third inst)))
			(setq c 3) (!))
		((trashes? b inst (defq info (pfind +opt_info_map (first inst)))) -1)
		((second info))))

(defun find-past-w (_ wop b i)
	;a read of it first, of any width, wants what is stored
	(find-past
		((and (eql wop (first inst)) (eql b (third inst)) (= i (last inst))) (!))
		((and (or (find (first inst) +opt_read_ops) (find (first inst) +opt_read_ops_float))
			(eql b (second inst)) (nql b (last inst)) (= i (third inst))) -1)
		((trashes? b inst (defq info (pfind +opt_info_map (first inst)))) -1)
		((third info))))

(defun find-past-rr (_ rs rd)
	(find-past
		((lmatch? inst `(emit-cpy-rr ,rs ,rd)) (!))
		((trashes? rs inst (defq info (pfind +opt_info_map (first inst)))) -1)
		((trashes? rd inst info) -1)
		((second info))))

(defun find-past-r (_ rd)
	(find-past
		((lmatch? inst `(emit-cpy-rr _ ,rd)) (!))
		((uses? rd inst) -1)
		((pfind +opt_read_kill_ops (first inst)))))

(defun find-past-cr (_ rd)
	(find-past
		((lmatch? inst `(emit-cpy-cr _ ,rd)) (!))
		((trashes? rd inst (defq info (pfind +opt_info_map (first inst)))) -1)
		((second info))))

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
	;one sweep, each instruction is looked up once, to see which pass, if
	;any, it is for
	(each! (lambda (inst)
		(when (defq e (pfind +opt_op_map (first inst)))
			(defq c (second e))
			(case (first e)
				(:read
					(opt-read-after-read-write)
					(if (pfind +opt_read_ops_map (first inst))
						(opt-read-after-cpy)))
				(:write (opt-write-after-write))
				(:branch (opt-redundant-branch))
				(:cpy (opt-redundant-cpy))
				(:ret (opt-tail-call)))))
		(list emit_list) emit_start emit_end))

;module
(export-symbols '(opt-emit-list))
(env-pop)