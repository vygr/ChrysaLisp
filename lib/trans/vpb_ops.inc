;the ops of the VP binary after the VP's own, +vp_emit_ops: those the
;prepasses of the translators make, numbered here so a translator's
;records can hold them. Those the arm64 prepass makes
(defq +vpb_arm64_ops ''(
	emit-arm64-ldp emit-arm64-ldp-i emit-arm64-ldp-ui emit-arm64-ldp-f
	emit-arm64-stp emit-arm64-stp-i emit-arm64-stp-f
	emit-arm64-add-rrr emit-arm64-sub-rrr emit-arm64-and-rrr emit-arm64-or-rrr emit-arm64-xor-rrr
	emit-arm64-shl-crr emit-arm64-shr-crr emit-arm64-asr-crr
	emit-arm64-add-crr emit-arm64-and-crr emit-arm64-or-crr emit-arm64-xor-crr
	emit-arm64-call-h emit-arm64-call-m emit-arm64-call-t
	emit-arm64-call-r-h emit-arm64-call-r-m emit-arm64-call-r-t
	emit-arm64-call-i-h emit-arm64-call-i-m emit-arm64-call-i-t
	emit-arm64-call-p-h emit-arm64-call-p-m emit-arm64-call-p-t))
;and those the x86_64 prepass makes
(defq +vpb_x86_64_ops ''(emit-x64-alu-ir))
;and those the riscv64 and la64 call fusions make, (vpb-call-fusion)
(defq +vpb_riscv64_ops ''(
	emit-riscv64-call-h emit-riscv64-call-m emit-riscv64-call-t
	emit-riscv64-call-r-h emit-riscv64-call-r-m emit-riscv64-call-r-t
	emit-riscv64-call-i-h emit-riscv64-call-i-m emit-riscv64-call-i-t
	emit-riscv64-call-p-h emit-riscv64-call-p-m emit-riscv64-call-p-t)
	+vpb_la64_ops ''(
	emit-la64-call-h emit-la64-call-m emit-la64-call-t
	emit-la64-call-r-h emit-la64-call-r-m emit-la64-call-r-t
	emit-la64-call-i-h emit-la64-call-i-m emit-la64-call-i-t
	emit-la64-call-p-h emit-la64-call-p-m emit-la64-call-p-t))
(defq +vpb_all_ops `'(~(last +vp_emit_ops) ~(last +vpb_arm64_ops) ~(last +vpb_x86_64_ops)
	~(last +vpb_riscv64_ops) ~(last +vpb_la64_ops)))
