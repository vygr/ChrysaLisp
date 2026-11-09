;canonical list of all VP emit opcode symbols

(defq +vp_emit_ops ''(
		cpy-cr add-cr sub-cr mul-cr and-cr or-cr
		xor-cr cpy-rr add-rr sub-rr mul-rr and-rr
		or-rr xor-rr shl-cr shr-cr asr-cr shl-rr
		shr-rr asr-rr sqrt-ff abs-ff swp-rr neg-ff
		div-rr div-rr-u min-cr max-cr min-rr max-rr
		abs-rr beq-ff bne-ff bge-ff blt-ff ble-ff
		bgt-ff cvt-rf cvt-fr cvt-fd cvt-df cpy-rf
		cpy-fr beq-cr bne-cr bge-cr ble-cr blt-cr
		bgt-cr beq-rr bne-rr bge-rr ble-rr blt-rr
		bgt-rr beq-dd bne-dd bge-dd blt-dd ble-dd
		bgt-dd call jmp call-p jmp-p call-r jmp-r
		call-rc jmp-rc load-rcr load-rcr-b load-rcr-s load-rcr-i
		load-rcr-ub load-rcr-us load-rcr-ui lea-rcr store-rrc
		store-rrc-b store-rrc-s store-rrc-i load-rrr load-rrr-b
		load-rrr-s load-rrr-i load-rrr-ub load-rrr-us load-rrr-ui
		lea-rrr store-rrr store-rrr-b store-rrr-s store-rrr-i load-pr
		lea-pr call-abi ret sync alloc free
		push pop stack-init save-state load-state entry-abi align nop trash label
		string byte short int long add-dd
		sub-dd mul-dd div-dd min-dd max-dd sqrt-dd
		abs-dd neg-dd cpy-dd cvt-rd cvt-dr cpy-rd
		cpy-dr load-rcd store-drc load-rrd store-drr
		load-rcf store-frc load-rrf store-frr rem-rr rem-rr-u
		;the single float ops, a register given as the double it is of, the
		;others in the places of ops that went, an op's place is its number
		add-ff sub-ff mul-ff div-ff min-ff max-ff
		;the nop the VP optimiser puts in place of an instruction it removes. It
		;is never emitted, but it is looked up, so it has its slot in every map
		;of the ops, or each lookup of it would search the whole map, and miss.
		vp-nop))
