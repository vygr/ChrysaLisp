;canonical list of all VP emit opcode symbols

(defq +vp_emit_ops ''(
	emit-cpy-cr emit-add-cr emit-sub-cr emit-mul-cr emit-and-cr emit-or-cr
	emit-xor-cr emit-cpy-rr emit-add-rr emit-sub-rr emit-mul-rr emit-and-rr
	emit-or-rr emit-xor-rr emit-shl-cr emit-shr-cr emit-asr-cr emit-shl-rr
	emit-shr-rr emit-asr-rr emit-lnot-rr emit-land-rr emit-swp-rr emit-ext-rr
	emit-div-rr emit-div-rr-u emit-min-cr emit-max-cr emit-min-rr emit-max-rr
	emit-abs-rr emit-seq-cr emit-sne-cr emit-slt-cr emit-sle-cr emit-sgt-cr
	emit-sge-cr emit-seq-rr emit-sne-rr emit-slt-rr emit-sle-rr emit-sgt-rr
	emit-sge-rr emit-beq-cr emit-bne-cr emit-bge-cr emit-ble-cr emit-blt-cr
	emit-bgt-cr emit-beq-rr emit-bne-rr emit-bge-rr emit-ble-rr emit-blt-rr
	emit-bgt-rr emit-beq-dd emit-bne-dd emit-bge-dd emit-blt-dd emit-ble-dd
	emit-bgt-dd emit-call emit-jmp emit-call-p emit-jmp-p emit-call-r emit-jmp-r
	emit-call-rc emit-jmp-rc emit-load-rcr emit-load-rcr-b emit-load-rcr-s emit-load-rcr-i
	emit-load-rcr-ub emit-load-rcr-us emit-load-rcr-ui emit-lea-rcr emit-store-rrc
	emit-store-rrc-b emit-store-rrc-s emit-store-rrc-i emit-load-rrr emit-load-rrr-b
	emit-load-rrr-s emit-load-rrr-i emit-load-rrr-ub emit-load-rrr-us emit-load-rrr-ui
	emit-lea-rrr emit-store-rrr emit-store-rrr-b emit-store-rrr-s emit-store-rrr-i emit-load-pr
	emit-lea-pr emit-call-abi emit-ret emit-sync emit-alloc emit-free
	emit-push emit-pop emit-stack-init emit-save-state emit-load-state emit-entry-abi emit-align emit-nop emit-trash emit-label
	emit-string emit-byte emit-short emit-int emit-long emit-add-dd
	emit-sub-dd emit-mul-dd emit-div-dd emit-min-dd emit-max-dd emit-sqrt-dd
	emit-abs-dd emit-neg-dd emit-cpy-dd emit-cvt-rd emit-cvt-dr emit-cpy-rd
	emit-cpy-dr emit-load-rcd emit-store-drc emit-load-rrd emit-store-drr
	emit-load-rcf emit-store-frc emit-load-rrf emit-store-frr emit-rem-rr emit-rem-rr-u
	;the single float ops, a register given as the double it is of
	emit-add-ff emit-sub-ff emit-mul-ff emit-div-ff emit-min-ff emit-max-ff
	emit-sqrt-ff emit-abs-ff emit-neg-ff emit-beq-ff emit-bne-ff emit-bge-ff
	emit-blt-ff emit-ble-ff emit-bgt-ff emit-cvt-rf emit-cvt-fr emit-cvt-fd
	emit-cvt-df emit-cpy-rf emit-cpy-fr
	;the nop the VP optimiser puts in place of an instruction it removes. It
	;is never emitted, but it is looked up, so it has its slot in every map
	;of the ops, or each lookup of it would search the whole map, and miss.
	emit-vp-nop
))