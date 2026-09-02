;canonical list of all VP emit opcode symbols

(defq +vp_emit_ops ''(
	emit-cpy-cr emit-add-cr emit-sub-cr emit-mul-cr emit-and-cr emit-or-cr
	emit-xor-cr emit-cpy-rr emit-add-rr emit-sub-rr emit-mul-rr emit-and-rr
	emit-or-rr emit-xor-rr emit-shl-cr emit-shr-cr emit-asr-cr emit-shl-rr
	emit-shr-rr emit-asr-rr emit-lnot-rr emit-land-rr emit-swp-rr emit-ext-rr
	emit-div-rrr emit-div-rrr-u emit-min-cr emit-max-cr emit-min-rr emit-max-rr
	emit-abs-rr emit-seq-cr emit-sne-cr emit-slt-cr emit-sle-cr emit-sgt-cr
	emit-sge-cr emit-seq-rr emit-sne-rr emit-slt-rr emit-sle-rr emit-sgt-rr
	emit-sge-rr emit-beq-cr emit-bne-cr emit-bge-cr emit-ble-cr emit-blt-cr
	emit-bgt-cr emit-beq-rr emit-bne-rr emit-bge-rr emit-ble-rr emit-blt-rr
	emit-bgt-rr emit-beq-ff emit-bne-ff emit-bge-ff emit-blt-ff emit-ble-ff
	emit-bgt-ff emit-call emit-jmp emit-call-p emit-jmp-p emit-call-r emit-jmp-r
	emit-call-i emit-jmp-i emit-cpy-ir emit-cpy-ir-b emit-cpy-ir-s emit-cpy-ir-i
	emit-cpy-ir-ub emit-cpy-ir-us emit-cpy-ir-ui emit-lea-i emit-cpy-ri
	emit-cpy-ri-b emit-cpy-ri-s emit-cpy-ri-i emit-cpy-dr emit-cpy-dr-b
	emit-cpy-dr-s emit-cpy-dr-i emit-cpy-dr-ub emit-cpy-dr-us emit-cpy-dr-ui
	emit-lea-d emit-cpy-rd emit-cpy-rd-b emit-cpy-rd-s emit-cpy-rd-i emit-cpy-pr
	emit-lea-p emit-call-abi emit-ret emit-sync emit-brk emit-alloc emit-free
	emit-push emit-pop emit-stack-init emit-align emit-nop emit-trash emit-label
	emit-string emit-byte emit-short emit-int emit-long emit-add-ff
	emit-sub-ff emit-mul-ff emit-div-ff emit-min-ff emit-max-ff emit-sqrt-ff
	emit-abs-ff emit-neg-ff emit-cpy-ff emit-cvt-rf emit-cvt-fr emit-cpy-rf
	emit-cpy-fr emit-cpy-if emit-cpy-fi emit-cpy-df emit-cpy-fd
))