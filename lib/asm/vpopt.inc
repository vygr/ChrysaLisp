;;;;;;;;;;;;;;
; VP optimizer
;;;;;;;;;;;;;;

;module
(env-push)

(defq
	+opt_nop
		''(vp-nop)
	+opt_call_ops
		''(call-p call-rc call call-r)
	+opt_jump_ops
		''(jmp-p jmp-rc jmp jmp-r)
	+opt_read_ops
		''(load-rcr load-rcr-ui load-rcr-i load-rcr-ub load-rcr-us load-rcr-b load-rcr-s)
	+opt_write_ops
		''(store-rrc store-rrc-i store-rrc-i store-rrc-b store-rrc-s store-rrc-b store-rrc-s)
	+opt_write_ops_signed
		''(store-rrc store-rrc-i store-rrc-b store-rrc-s)
	;those of a float, with an offset where an int op has it
	+opt_read_ops_float
		''(load-rcd load-rcf)
	+opt_write_ops_float
		''(store-drc store-frc)
	+opt_branch_ops
		''(beq-cr bne-cr bge-cr ble-cr blt-cr bgt-cr))

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
					(if (find %1 '(label call-p free alloc push pop save-state load-state entry-abi
								call-rc call-abi call call-r)) -1)))
			+vp_emit_ops (pmap))
	+opt_write_kill_ops
		(reduce (# (pinsert %0 %1
					(if (find %1 '(label call-p free alloc push pop save-state load-state entry-abi
								call-rc call-abi call call-r
								jmp beq-cr bne-cr bne-rr beq-rr bge-rr
								ble-rr jmp-p bge-cr ble-cr blt-cr blt-rr
								bgt-rr bgt-cr jmp-r jmp-rc beq-dd bne-dd
								blt-dd bgt-dd ble-dd bge-dd
								beq-ff bne-ff blt-ff bgt-ff ble-ff bge-ff)) -1)))
			+vp_emit_ops (pmap))
	+opt_out_idx_map
		(reduce (# (pinsert %0 %1
					(cond
						((find %1 '(pop swp-rr)) :multi)
						((find %1 '(cpy-cr add-cr sub-cr mul-cr and-cr or-cr xor-cr
									cpy-rr add-rr sub-rr mul-rr and-rr or-rr xor-rr
									div-rr div-rr-u rem-rr rem-rr-u
									shl-cr shr-cr asr-cr shl-rr shr-rr asr-rr
									min-cr max-cr min-rr max-rr abs-rr
									load-rcr load-rcr-b load-rcr-s load-rcr-i load-rcr-ub load-rcr-us load-rcr-ui lea-rcr
									load-rrr load-rrr-b load-rrr-s load-rrr-i load-rrr-ub load-rrr-us load-rrr-ui lea-rrr
									load-pr lea-pr
									add-dd sub-dd mul-dd div-dd min-dd max-dd
									sqrt-dd abs-dd neg-dd cpy-dd
									cvt-rd cvt-dr cpy-rd cpy-dr
									add-ff sub-ff mul-ff div-ff min-ff max-ff
									sqrt-ff abs-ff neg-ff cvt-fd cvt-df
									cvt-rf cvt-fr cpy-rf cpy-fr
									load-rcd load-rrd load-rcf load-rrf)) -2))))
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
						((eql %1 'cpy-rr) (list :cpy 0))
						((eql %1 'ret) (list :ret 0)))))
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
			(if (and (eql wop (first inst)) (eql wop 'store-rrc)) (progn (setq c 1) (!)) -1))
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
		((lmatch? inst `(cpy-rr ,rs ,rd)) (!))
		((trashes? rs inst (defq info (pfind +opt_info_map (first inst)))) -1)
		((trashes? rd inst info) -1)
		((second info))))

(defun find-past-r (_ rd)
	(find-past
		((lmatch? inst `(cpy-rr _ ,rd)) (!))
		((uses? rd inst) -1)
		((pfind +opt_read_kill_ops (first inst)))))

(defun find-past-cr (_ rd)
	(find-past
		((lmatch? inst `(cpy-cr _ ,rd)) (!))
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
					(setq inst (list 'cpy-rr rs (last inst))))))))

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
