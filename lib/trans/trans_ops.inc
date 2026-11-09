;the ops of a translator's records after the VP's own, +vp_emit_ops:
;those the prepasses of the translators make, numbered here so the
;records can hold them. Those the arm64 prepass makes
(defq +trans_arm64_ops ''(
		arm64-ldp arm64-ldp-i arm64-ldp-ui arm64-ldp-f
		arm64-stp arm64-stp-i arm64-stp-f
		arm64-add-rrr arm64-sub-rrr arm64-and-rrr arm64-or-rrr arm64-xor-rrr
		arm64-shl-crr arm64-shr-crr arm64-asr-crr
		arm64-add-crr arm64-and-crr arm64-or-crr arm64-xor-crr
		arm64-call-h arm64-call-m arm64-call-t
		arm64-call-r-h arm64-call-r-m arm64-call-r-t
		arm64-call-rc-h arm64-call-rc-m arm64-call-rc-t
		arm64-call-p-h arm64-call-p-m arm64-call-p-t))
;and those the x86_64 prepass makes
(defq +trans_x86_64_ops ''(x64-alu-ir))
;and those the riscv64 and la64 call fusions make, (trans-call-fusion)
(defq +trans_riscv64_ops ''(
		riscv64-call-h riscv64-call-m riscv64-call-t
		riscv64-call-r-h riscv64-call-r-m riscv64-call-r-t
		riscv64-call-rc-h riscv64-call-rc-m riscv64-call-rc-t
		riscv64-call-p-h riscv64-call-p-m riscv64-call-p-t)
	+trans_la64_ops ''(
		la64-call-h la64-call-m la64-call-t
		la64-call-r-h la64-call-r-m la64-call-r-t
		la64-call-rc-h la64-call-rc-m la64-call-rc-t
		la64-call-p-h la64-call-p-m la64-call-p-t))
(defq +trans_ops `'(~(last +vp_emit_ops) ~(last +trans_arm64_ops) ~(last +trans_x86_64_ops)
		~(last +trans_riscv64_ops) ~(last +trans_la64_ops)))
