#include <string.h>
#include <sys/types.h>
#include <stdio.h>
#include <atomic>
#include <cmath>
#ifdef _WIN64
	#include <immintrin.h>
	#include <intrin.h>
#endif
#include "pii.h"

// the ops in groups, each of its forms after its first, and lib/trans/vp64.vp
// has them in this order. X(name) is of each, vp64_name its number
#define VP64_OPS(X) \
	/* integer, of a constant */ \
	X(cpy_cr_0) X(cpy_cr_1) X(cpy_cr_2) X(cpy_cr_3) X(add_cr_0) X(add_cr_1) \
	X(add_cr_2) X(add_cr_3) X(sub_cr_0) X(sub_cr_1) X(sub_cr_2) X(sub_cr_3) \
	X(mul_cr_0) X(mul_cr_1) X(mul_cr_2) X(mul_cr_3) X(and_cr_0) X(and_cr_1) \
	X(and_cr_2) X(and_cr_3) X(or_cr_0) X(or_cr_1) X(or_cr_2) X(or_cr_3) \
	X(xor_cr_0) X(xor_cr_1) X(xor_cr_2) X(xor_cr_3) X(min_cr_0) X(min_cr_1) \
	X(min_cr_2) X(min_cr_3) X(max_cr_0) X(max_cr_1) X(max_cr_2) X(max_cr_3) \
	X(shl_cr) X(shr_cr) X(asr_cr) \
	/* integer, of a register */ \
	X(cpy_rr) X(add_rr) X(sub_rr) X(mul_rr) X(and_rr) X(or_rr) \
	X(xor_rr) X(shl_rr) X(shr_rr) X(asr_rr) X(min_rr) X(max_rr) \
	X(abs_rr) X(swp_rr) X(div_rr) X(div_rr_u) X(rem_rr) X(rem_rr_u) \
	/* integer compare and branch */ \
	X(cmp_cr_0) X(cmp_cr_1) X(cmp_cr_2) X(cmp_cr_3) X(cmp_rr) X(beq_0) \
	X(beq_1) X(bne_0) X(bne_1) X(bge_0) X(bge_1) X(blt_0) \
	X(blt_1) X(ble_0) X(ble_1) X(bgt_0) X(bgt_1) \
	/* integer memory */ \
	X(load_rcr) X(load_rcr_w) X(load_rcr_b) X(load_rcr_b_w) X(load_rcr_s) X(load_rcr_s_w) \
	X(load_rcr_i) X(load_rcr_i_w) X(load_rcr_ub) X(load_rcr_ub_w) X(load_rcr_us) X(load_rcr_us_w) \
	X(load_rcr_ui) X(load_rcr_ui_w) X(lea_rcr) X(lea_rcr_w) X(store_rrc) X(store_rrc_w) \
	X(store_rrc_b) X(store_rrc_b_w) X(store_rrc_s) X(store_rrc_s_w) X(store_rrc_i) X(store_rrc_i_w) \
	X(load_rrr) X(load_rrr_b) X(load_rrr_s) X(load_rrr_i) X(load_rrr_ub) X(load_rrr_us) \
	X(load_rrr_ui) X(lea_rrr) X(store_rrr) X(store_rrr_b) X(store_rrr_s) X(store_rrr_i) \
	/* calls, jumps and the system */ \
	X(call_0) X(call_1) X(jmp_0) X(jmp_1) X(call_p_0) X(call_p_1) \
	X(jmp_p_0) X(jmp_p_1) X(call_r) X(jmp_r) X(call_rc) X(jmp_rc) \
	X(ret) X(call_abi) X(load_pr) X(lea_pr) X(sync) \
	/* doubles */ \
	X(add_dd) X(sub_dd) X(mul_dd) X(div_dd) X(min_dd) X(max_dd) \
	X(sqrt_dd) X(abs_dd) X(neg_dd) X(cpy_dd) X(cvt_rd) X(cvt_dr) \
	X(cpy_rd) X(cpy_dr) X(cmp_dd) X(fbeq_0) X(fbeq_1) X(fbne_0) \
	X(fbne_1) X(fbge_0) X(fbge_1) X(fblt_0) X(fblt_1) X(fble_0) \
	X(fble_1) X(fbgt_0) X(fbgt_1) X(load_rcd) X(load_rcd_w) X(store_drc) \
	X(store_drc_w) X(load_rrd) X(store_drr) \
	/* singles, a register the double it is of */ \
	X(add_ff) X(sub_ff) X(mul_ff) X(div_ff) X(min_ff) X(max_ff) \
	X(sqrt_ff) X(abs_ff) X(neg_ff) X(cvt_rf) X(cvt_fr) X(cvt_fd) \
	X(cvt_df) X(cpy_rf) X(cpy_fr) X(cmp_ff) X(load_rcf) X(load_rcf_w) \
	X(store_frc) X(store_frc_w) X(load_rrf) X(store_frr)

#define VP64_ENUM(o) vp64_##o,
enum Opcodes { VP64_OPS(VP64_ENUM) vp64_count };


// generic operations
#define vp_op(op, s, d) d = d op s
#define vp_op_cr(op, c, dr) vp_op(op, c, regs[dr])
#define vp_op_rr(op, sr, dr) vp_op(op, regs[sr], regs[dr])
#define vp_op_dd(op, sr, dr) vp_op(op, fregs[sr], fregs[dr])

#define vp_cpy_cr(c, dr) regs[dr] = (c)
#define vp_add_cr(c, dr) vp_op_cr(+, c, dr)
#define vp_sub_cr(c, dr) vp_op_cr(-, c, dr)
#define vp_cmp_cr(c, dr) { compare1 = regs[dr]; compare2 = (c); }
#define vp_mul_cr(c, dr) vp_op_cr(*, c, dr)
#define vp_and_cr(c, dr) vp_op_cr(&, c, dr)
#define vp_or_cr(c, dr) vp_op_cr(|, c, dr)
#define vp_xor_cr(c, dr) vp_op_cr(^, c, dr)
// a shift means the one thing on every CPU, as a divide does. It is by the
// low 6 bits of what it is given, so by 64 is by 0 and by -1 is by 63, which
// is what x86-64, ARM64 and RISC-V each do with a count in a register. Left
// to C++ a shift of 64 or more has no meaning, and what came of it was the
// compiler's to choose
#define vp_shl_cr(c, dr) regs[dr] = (int64_t)((uint64_t)regs[dr] << ((c) & 63))
#define vp_shr_cr(c, dr) regs[dr] = (int64_t)((uint64_t)regs[dr] >> ((c) & 63))
#define vp_asr_cr(c, dr) regs[dr] = regs[dr] >> ((c) & 63)
#define vp_min_cr(c, dr) { int64_t _c = (c); if (_c < regs[dr]) regs[dr] = _c; }
#define vp_max_cr(c, dr) { int64_t _c = (c); if (_c > regs[dr]) regs[dr] = _c; }

#define vp_cpy_rr(sr, dr) regs[dr] = regs[sr]
#define vp_add_rr(sr, dr) vp_op_rr(+, sr, dr)
#define vp_sub_rr(sr, dr) vp_op_rr(-, sr, dr)
#define vp_cmp_rr(sr, dr) { compare1 = regs[dr]; compare2 = regs[sr]; }
#define vp_mul_rr(sr, dr) vp_op_rr(*, sr, dr)
#define vp_and_rr(sr, dr) vp_op_rr(&, sr, dr)
#define vp_or_rr(sr, dr) vp_op_rr(|, sr, dr)
#define vp_xor_rr(sr, dr) vp_op_rr(^, sr, dr)
#define vp_shl_rr(sr, dr) regs[dr] = (int64_t)((uint64_t)regs[dr] << (regs[sr] & 63))
#define vp_shr_rr(sr, dr) regs[dr] = (int64_t)((uint64_t)regs[dr] >> (regs[sr] & 63))
#define vp_asr_rr(sr, dr) regs[dr] = regs[dr] >> (regs[sr] & 63)
#define vp_swp_rr(sr, dr) { int64_t t = regs[dr]; regs[dr] = regs[sr]; regs[sr] = t; }

// a real made an integer means the one thing too, and here the CPUs do not
// agree: one too big to be an integer is the most negative number on x86-64
// and the biggest on ARM64, and what is not a number is that or 0. Left to
// C++ it has no meaning. Here it is toward 0, the biggest or the most
// negative integer for one past either, and 0 for what is not a number
static inline int64_t vp_f2i(double f)
{
	if (f != f) return 0;
	if (f >= 9223372036854775808.0) return INT64_MAX;
	if (f <= -9223372036854775808.0) return INT64_MIN;
	return (int64_t)f;
}

// a divide and a remainder mean the one thing on every CPU, whatever the CPU
// would do itself. Each takes a number of 64 bits and writes the one
// register. By 0 the answer is 0 and what is left over is the number. The
// most negative number by -1 is itself, with nothing left over.
#define vp_div_rr(sr, dr) { int64_t n = regs[dr]; int64_t d = regs[sr]; if (!d) regs[dr] = 0; else if (d == -1) regs[dr] = (int64_t)(0 - (uint64_t)n); else regs[dr] = n / d; }
#define vp_div_rr_u(sr, dr) { uint64_t n = (uint64_t)regs[dr]; uint64_t d = (uint64_t)regs[sr]; regs[dr] = d ? (int64_t)(n / d) : 0; }
#define vp_rem_rr(sr, dr) { int64_t n = regs[dr]; int64_t d = regs[sr]; if (d == -1) regs[dr] = 0; else if (d) regs[dr] = n % d; }
#define vp_rem_rr_u(sr, dr) { uint64_t n = (uint64_t)regs[dr]; uint64_t d = (uint64_t)regs[sr]; if (d) regs[dr] = (int64_t)(n % d); }

#define vp_min_rr(sr, dr) if (regs[sr] < regs[dr]) regs[dr] = regs[sr]
#define vp_max_rr(sr, dr) if (regs[sr] > regs[dr]) regs[dr] = regs[sr]
#define vp_abs_rr(sr, dr) { int64_t t = regs[sr]; regs[dr] = (t < 0) ? -t : t; }

// validation mode, run_tui.sh -V: each load and store is at its natural
// alignment, docs/vm/vp_vm.md, or the place it is in is said, the once, the
// function by the header found back from pc. The interpreter is made twice,
// vp64_run<true> and vp64_run<false>, so a run without it pays nothing
bool vp64_validate = false;
static int64_t vp64_al_seen[4096];
static int vp64_al_n = 0;
static void vp64_unaligned(int16_t* pc, int64_t ir, int64_t a, int size)
{
	int64_t key = (int64_t)pc;
	for (int i = 0; i < vp64_al_n; i++) if (vp64_al_seen[i] == key) return;
	if (vp64_al_n < 4096) vp64_al_seen[vp64_al_n++] = key;
	const char *name = "?"; int at = -1;
	for (uint8_t *h = (uint8_t*)((int64_t)pc & ~7LL); h > (uint8_t*)pc - 65536; h -= 8)
	{
		uint16_t len = *(uint16_t*)(h + 8), entry = *(uint16_t*)(h + 10);
		if (h + len <= (uint8_t*)pc || h + entry > (uint8_t*)pc || entry < 20) continue;
		const char *n = (const char*)h + 18; int k = 0;
		while (k < 120 && n[k] >= 32 && n[k] < 127) k++;
		if (k > 2 && n[k] == 0 && strchr(n, '/')) { name = n; at = (int)((uint8_t*)pc - h - entry); break; }
	}
	fprintf(stderr, "VP64 UNALIGNED %s +%d, %d bytes at %llx, opcode %d\n",
		name, at, size, (long long)a, (int)(ir & 0xff));
}
#define vp_al(a, t) if (V) { if ((a) & (sizeof(t) - 1)) vp64_unaligned(pc, ir, (a), (int)sizeof(t)); }

#define vp_mem_ir(sr, o, dr, t) { int64_t _m = regs[sr] + (o); vp_al(_m, t) regs[dr] = (int64_t)*(t*)_m; }
#define vp_mem_ri(sr, dr, o, t) { int64_t _m = regs[dr] + (o); vp_al(_m, t) *(t*)_m = (t)regs[sr]; }
#define vp_lea_rcr(sr, o, dr) regs[dr] = regs[sr] + o
#define vp_load_rcr_b(sr, o, dr) vp_mem_ir(sr, o, dr, int8_t)
#define vp_load_rcr_s(sr, o, dr) vp_mem_ir(sr, o, dr, int16_t)
#define vp_load_rcr_i(sr, o, dr) vp_mem_ir(sr, o, dr, int32_t)
#define vp_load_rcr(sr, o, dr) vp_mem_ir(sr, o, dr, int64_t)
#define vp_load_rcr_ub(sr, o, dr) vp_mem_ir(sr, o, dr, uint8_t)
#define vp_load_rcr_us(sr, o, dr) vp_mem_ir(sr, o, dr, uint16_t)
#define vp_load_rcr_ui(sr, o, dr) vp_mem_ir(sr, o, dr, uint32_t)
#define vp_store_rrc_b(sr, dr, o) vp_mem_ri(sr, dr, o, int8_t)
#define vp_store_rrc_s(sr, dr, o) vp_mem_ri(sr, dr, o, int16_t)
#define vp_store_rrc_i(sr, dr, o) vp_mem_ri(sr, dr, o, int32_t)
#define vp_store_rrc(sr, dr, o) vp_mem_ri(sr, dr, o, int64_t)

#define vp_mem_dr(b, sr, dr, t) { int64_t _m = regs[b] + regs[sr]; vp_al(_m, t) regs[dr] = (int64_t)*(t*)_m; }
#define vp_mem_rd(sr, b, dr, t) { int64_t _m = regs[b] + regs[dr]; vp_al(_m, t) *(t*)_m = (t)regs[sr]; }
#define vp_lea_rrr(b, sr, dr) regs[dr] = regs[b] + regs[sr]
#define vp_store_rrr_b(sr, b, dr) vp_mem_rd(sr, b, dr, uint8_t)
#define vp_store_rrr_s(sr, b, dr) vp_mem_rd(sr, b, dr, uint16_t)
#define vp_store_rrr_i(sr, b, dr) vp_mem_rd(sr, b, dr, uint32_t)
#define vp_store_rrr(sr, b, dr) vp_mem_rd(sr, b, dr, uint64_t)
#define vp_load_rrr_b(b, sr, dr) vp_mem_dr(b, sr, dr, int8_t)
#define vp_load_rrr_s(b, sr, dr) vp_mem_dr(b, sr, dr, int16_t)
#define vp_load_rrr_i(b, sr, dr) vp_mem_dr(b, sr, dr, int32_t)
#define vp_load_rrr(b, sr, dr) vp_mem_dr(b, sr, dr, int64_t)
#define vp_load_rrr_ub(b, sr, dr) vp_mem_dr(b, sr, dr, uint8_t)
#define vp_load_rrr_us(b, sr, dr) vp_mem_dr(b, sr, dr, uint16_t)
#define vp_load_rrr_ui(b, sr, dr) vp_mem_dr(b, sr, dr, uint32_t)

#define vp_pc_rel(o) (int16_t*)((char*)pc + (o))
#define vp_pc_ind(a) *(int16_t**)(a)
#define vp_push_pc() { regs[15] -= 8; *(int16_t**)regs[15] = pc; }
#define vp_pop_pc() { pc = *(int16_t**)regs[15]; regs[15] += 8; }

#define vp_branch(c, o) { int64_t _o = (o); if (c) pc = vp_pc_rel(_o); }
#define vp_beq(o) vp_branch(compare1 == compare2, o)
#define vp_bne(o) vp_branch(compare1 != compare2, o)
#define vp_bge(o) vp_branch(compare1 >= compare2, o)
#define vp_blt(o) vp_branch(compare1 < compare2, o)
#define vp_ble(o) vp_branch(compare1 <= compare2, o)
#define vp_bgt(o) vp_branch(compare1 > compare2, o)
#define vp_call(o) { int64_t _o = (o); vp_push_pc(); pc = vp_pc_rel(_o); }
#define vp_jmp(o) { int64_t _o = (o); pc = vp_pc_rel(_o); }
#define vp_call_p(o) { int64_t _o = (o); vp_push_pc(); pc = vp_pc_ind(vp_pc_rel(_o)); }
#define vp_jmp_p(o) { int64_t _o = (o); pc = vp_pc_ind(vp_pc_rel(_o)); }
#define vp_call_r(dr) { vp_push_pc(); pc = (int16_t*)regs[dr]; }
#define vp_jmp_r(dr) pc = (int16_t*)regs[dr]
#define vp_call_rc(br, o) { int64_t b = regs[br], _o = (o); vp_push_pc(); pc = vp_pc_ind(b + _o); }
#define vp_jmp_rc(br, o) { int64_t b = regs[br], _o = (o); pc = vp_pc_ind(b + _o); }
#define vp_load_pr(o, dr) { int64_t _o = (o); regs[dr] = *(int64_t*)vp_pc_rel(_o); }
#define vp_lea_pr(o, dr) { int64_t _o = (o); regs[dr] = (int64_t)vp_pc_rel(_o); }
#define vp_ret() vp_pop_pc()

#define vp_sync(c) sync.store((c), std::memory_order_seq_cst)

#define vp_call_abi(n, b, o) { \
	int64_t f = *(uint64_t*)(regs[b] + (o)); \
	switch (n) { \
		case 0: regs[0] = (((uint64_t(*)(void))f)()); break; \
		case 1: regs[0] = (((uint64_t(*)(uint64_t))f)(regs[0])); break; \
		case 2: regs[0] = (((uint64_t(*)(uint64_t, uint64_t))f)(regs[0], regs[1])); break; \
		case 3: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t))f)(regs[0], regs[1], regs[2])); break; \
		case 4: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t))f)(regs[0], regs[1], regs[2], regs[3])); break; \
		case 5: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))f)(regs[0], regs[1], regs[2], regs[3], regs[4])); break; \
		case 6: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5])); break; \
		case 7: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6])); break; \
		case 8: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7])); break; \
		case 9: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8])); break; \
		case 10: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9])); break; \
		case 11: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10])); break; \
		case 12: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10], regs[11])); break; \
		case 13: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10], regs[11], regs[12])); break; \
		case 14: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10], regs[11], regs[12], regs[13])); break; \
		case 15: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) f)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10], regs[11], regs[12], regs[13], regs[14])); break; \
	} \
}

#define vp_add_dd(sr, dr) vp_op_dd(+, sr, dr)
#define vp_sub_dd(sr, dr) vp_op_dd(-, sr, dr)
#define vp_mul_dd(sr, dr) vp_op_dd(*, sr, dr)
#define vp_div_dd(sr, dr) vp_op_dd(/, sr, dr)
#define vp_min_dd(sr, dr) fregs[dr] = std::fmin(fregs[dr], fregs[sr])
#define vp_max_dd(sr, dr) fregs[dr] = std::fmax(fregs[dr], fregs[sr])
#define vp_sqrt_dd(sr, dr) fregs[dr] = std::sqrt(fregs[sr])
#define vp_abs_dd(sr, dr) fregs[dr] = std::abs(fregs[sr])
#define vp_neg_dd(sr, dr) fregs[dr] = -fregs[sr]

#define vp_fbranch(c, o) { int64_t _o = (o); if (c) pc = vp_pc_rel(_o); }
#define vp_fbeq(o) vp_fbranch(compare_f1 == compare_f2, o)
#define vp_fbne(o) vp_fbranch(compare_f1 != compare_f2, o)
#define vp_fbge(o) vp_fbranch(compare_f2 >= compare_f1, o)
#define vp_fblt(o) vp_fbranch(compare_f2 < compare_f1, o)
#define vp_fble(o) vp_fbranch(compare_f2 <= compare_f1, o)
#define vp_fbgt(o) vp_fbranch(compare_f2 > compare_f1, o)

#define vp_mem_if(br, o, dr, t) { int64_t _m = regs[br] + (o); vp_al(_m, t) fregs[dr] = (double)*(t*)_m; }
#define vp_mem_fi(sr, br, o, t) { int64_t _m = regs[br] + (o); vp_al(_m, t) *(t*)_m = (t)fregs[sr]; }
#define vp_mem_df(br, ix, dr, t) { int64_t _m = regs[br] + regs[ix]; vp_al(_m, t) fregs[dr] = (double)*(t*)_m; }
#define vp_mem_fd(sr, br, ix, t) { int64_t _m = regs[br] + regs[ix]; vp_al(_m, t) *(t*)_m = (t)fregs[sr]; }

#define vp_load_rcd(br, o, dr) vp_mem_if(br, o, dr, double)
#define vp_store_drc(sr, br, o) vp_mem_fi(sr, br, o, double)
#define vp_load_rrd(br, ir, dr) vp_mem_df(br, ir, dr, double)
#define vp_store_drr(sr, br, ir) vp_mem_fd(sr, br, ir, double)

//a single is the low 32 bits of a float register, the high 32 bits 0, its
//bits as they are, by memcpy
static inline float vp_sget(const double *f)
{
	float s;
	memcpy(&s, f, sizeof(float));
	return s;
}

static inline void vp_sset(double *f, float s)
{
	uint32_t w;
	memcpy(&w, &s, sizeof(float));
	uint64_t b = w;
	memcpy(f, &b, sizeof(uint64_t));
}

#define vp_mem_is(br, o, dr) { int64_t _m = regs[br] + (o); vp_al(_m, float) vp_sset(&fregs[dr], *(float*)_m); }
#define vp_mem_si(sr, br, o) { int64_t _m = regs[br] + (o); vp_al(_m, float) *(float*)_m = vp_sget(&fregs[sr]); }
#define vp_load_rcf(br, o, dr) vp_mem_is(br, o, dr)
#define vp_store_frc(sr, br, o) vp_mem_si(sr, br, o)
#define vp_load_rrf(br, ir, dr) vp_mem_is(br, regs[ir], dr)
#define vp_store_frr(sr, br, ir) vp_mem_si(sr, br, regs[ir])

#define vp_op_ff(op, sr, dr) vp_sset(&fregs[dr], vp_sget(&fregs[dr]) op vp_sget(&fregs[sr]))
#define vp_add_ff(sr, dr) vp_op_ff(+, sr, dr)
#define vp_sub_ff(sr, dr) vp_op_ff(-, sr, dr)
#define vp_mul_ff(sr, dr) vp_op_ff(*, sr, dr)
#define vp_div_ff(sr, dr) vp_op_ff(/, sr, dr)
#define vp_min_ff(sr, dr) vp_sset(&fregs[dr], std::fmin(vp_sget(&fregs[dr]), vp_sget(&fregs[sr])))
#define vp_max_ff(sr, dr) vp_sset(&fregs[dr], std::fmax(vp_sget(&fregs[dr]), vp_sget(&fregs[sr])))
#define vp_sqrt_ff(sr, dr) vp_sset(&fregs[dr], std::sqrt(vp_sget(&fregs[sr])))
#define vp_abs_ff(sr, dr) vp_sset(&fregs[dr], std::abs(vp_sget(&fregs[sr])))
#define vp_neg_ff(sr, dr) vp_sset(&fregs[dr], -vp_sget(&fregs[sr]))
//a single is a double exactly, so the compare is of the doubles
#define vp_cmp_ff(sr, dr) { compare_f1 = vp_sget(&fregs[sr]); compare_f2 = vp_sget(&fregs[dr]); }
#define vp_cvt_rf(sr, dr) vp_sset(&fregs[dr], (float)regs[sr])
#define vp_cvt_fr(sr, dr) regs[dr] = vp_f2i(vp_sget(&fregs[sr]))
#define vp_cvt_fd(sr, dr) fregs[dr] = vp_sget(&fregs[sr])
#define vp_cvt_df(sr, dr) vp_sset(&fregs[dr], (float)fregs[sr])
#define vp_cpy_rf(sr, dr) { uint32_t _w = (uint32_t)regs[sr]; float _s; memcpy(&_s, &_w, sizeof(float)); vp_sset(&fregs[dr], _s); }
#define vp_cpy_fr(sr, dr) { uint32_t _w; float _s = vp_sget(&fregs[sr]); memcpy(&_w, &_s, sizeof(float)); regs[dr] = _w; }

#define vp_cvt_rd(sr, dr) fregs[dr] = (double)regs[sr]
#define vp_cvt_dr(sr, dr) regs[dr] = vp_f2i(fregs[sr])
#define vp_cpy_dd(sr, dr) fregs[dr] = fregs[sr]
//the bits as they are, by memcpy, which is a move, a cast of the pointer
//is not to be relied on at -O3
#define vp_cpy_rd(sr, dr) memcpy(&fregs[dr], &regs[sr], sizeof(int64_t))
#define vp_cpy_dr(sr, dr) memcpy(&regs[dr], &fregs[sr], sizeof(int64_t))
#define vp_cmp_dd(sr, dr) { compare_f1 = fregs[sr]; compare_f2 = fregs[dr]; }

#define vd_dr() ((ir >> 8) & 0xf)
#define vd_sr() ((ir >> 12) & 0xf)
#define vd_c0() ((int64_t)ir >> 12)
#define vd_c1() (vd_sr() | (vd_i() << 4))
#define vd_c2() ([&]{ uint64_t o1 = vd_sc() << 4; int64_t o2 = vd_i() << 20; return (int64_t)(vd_sr() | o1 | o2); }())
#define vd_c3() ([&]{ uint64_t o0 = vd_sc(); uint64_t o1 = vd_sc() << 16; uint64_t o2 = vd_sc() << 32; int64_t o3 = vd_i() << 48; return (int64_t)(o0 | o1 | o2 | o3); }())
#define vd_sc() ((uint64_t)*(uint16_t*)pc++)
#define vd_i() ((int64_t)*pc++)
#define vd_o0() ((int8_t)(ir >> 8))
#define vd_o1() (((ir >> 8) & 0xff) + ((int64_t)*pc++ << 8))

// each op is a label, op_name, its code, then the jump to the next op's
// label by its number, so each op has its own jump for the CPU to learn
#ifdef VP64_SWITCH
#define NEXT break;
#define OP(o) case vp64_##o:
#else
#define NEXT { ir = *pc++; goto *ops[ir & 0xff]; }
#define OP(o) op_##o:
#endif
#define CR4(o) \
	OP(o##_cr_0) vp_##o##_cr(vd_c0(), vd_dr()); NEXT \
	OP(o##_cr_1) vp_##o##_cr(vd_c1(), vd_dr()); NEXT \
	OP(o##_cr_2) vp_##o##_cr(vd_c2(), vd_dr()); NEXT \
	OP(o##_cr_3) vp_##o##_cr(vd_c3(), vd_dr()); NEXT
#define RR(o, t) OP(o##_##t) vp_##o##_##t(vd_sr(), vd_dr()); NEXT
#define B2(o) \
	OP(o##_0) vp_##o(vd_o0()); NEXT \
	OP(o##_1) vp_##o(vd_o1()); NEXT
#define IR2(o) \
	OP(o) vp_##o(vd_sr(), vd_i(), vd_dr()); NEXT \
	OP(o##_w) vp_##o(vd_sr(), vd_c3(), vd_dr()); NEXT
#define RI2(o) \
	OP(o) vp_##o(vd_sr(), vd_dr(), vd_i()); NEXT \
	OP(o##_w) vp_##o(vd_sr(), vd_dr(), vd_c3()); NEXT
#define DR(o) OP(o) vp_##o(vd_sr(), vd_sc(), vd_dr()); NEXT
#define RD(o) OP(o) vp_##o(vd_sc(), vd_dr(), vd_sr()); NEXT

template <bool V>
static int vp64_run(uint8_t *data, int64_t *stack, int64_t *argv, void **host_calls)
{
	int64_t regs[16];
	double fregs[16];
	int16_t* pc;
	int64_t ir;
	int64_t compare1 = 0;
	int64_t compare2 = 0;
	double compare_f1 = 0.0;
	double compare_f2 = 0.0;
	std::atomic<int> sync;

	//the label of each op by its number, any other number is not an op
#ifndef VP64_SWITCH
	void *ops[256];
	for (int i = 0; i < 256; i++) ops[i] = &&op_bad;
	#define VP64_LABEL(o) ops[vp64_##o] = &&op_##o;
	VP64_OPS(VP64_LABEL)
#endif

	fn_header* pHeader = (fn_header*)data;
	pc = (int16_t*)((uint8_t*)data + pHeader->entry);

	regs[0] = (int64_t)argv;
	regs[1] = (int64_t)host_calls;
	regs[15] = (int64_t)stack;

#ifdef VP64_SWITCH
	for (;;) { ir = *pc++; switch (ir & 0xff) {
#else
	NEXT
#endif
	// integer, of a constant
	CR4(cpy)
	CR4(add)
	CR4(sub)
	CR4(mul)
	CR4(and)
	CR4(or)
	CR4(xor)
	CR4(min)
	CR4(max)
	OP(shl_cr) vp_shl_cr(vd_sc(), vd_dr()); NEXT
	OP(shr_cr) vp_shr_cr(vd_sc(), vd_dr()); NEXT
	OP(asr_cr) vp_asr_cr(vd_sc(), vd_dr()); NEXT
	// integer, of a register
	RR(cpy, rr)
	RR(add, rr)
	RR(sub, rr)
	RR(mul, rr)
	RR(and, rr)
	RR(or, rr)
	RR(xor, rr)
	RR(shl, rr)
	RR(shr, rr)
	RR(asr, rr)
	RR(min, rr)
	RR(max, rr)
	RR(abs, rr)
	RR(swp, rr)
	RR(div, rr)
	RR(div, rr_u)
	RR(rem, rr)
	RR(rem, rr_u)
	// integer compare and branch
	CR4(cmp)
	RR(cmp, rr)
	B2(beq)
	B2(bne)
	B2(bge)
	B2(blt)
	B2(ble)
	B2(bgt)
	// integer memory
	IR2(load_rcr)
	IR2(load_rcr_b)
	IR2(load_rcr_s)
	IR2(load_rcr_i)
	IR2(load_rcr_ub)
	IR2(load_rcr_us)
	IR2(load_rcr_ui)
	IR2(lea_rcr)
	RI2(store_rrc)
	RI2(store_rrc_b)
	RI2(store_rrc_s)
	RI2(store_rrc_i)
	DR(load_rrr)
	DR(load_rrr_b)
	DR(load_rrr_s)
	DR(load_rrr_i)
	DR(load_rrr_ub)
	DR(load_rrr_us)
	DR(load_rrr_ui)
	DR(lea_rrr)
	RD(store_rrr)
	RD(store_rrr_b)
	RD(store_rrr_s)
	RD(store_rrr_i)
	// calls, jumps and the system
	B2(call)
	B2(jmp)
	B2(call_p)
	B2(jmp_p)
	OP(call_r) vp_call_r(vd_dr()); NEXT
	OP(jmp_r) vp_jmp_r(vd_dr()); NEXT
	OP(call_rc) vp_call_rc(vd_dr(), vd_c1()); NEXT
	OP(jmp_rc) vp_jmp_rc(vd_dr(), vd_c1()); NEXT
	OP(ret) vp_ret(); NEXT
	OP(call_abi) vp_call_abi(vd_sr(), vd_dr(), vd_sc()); NEXT
	OP(load_pr) vp_load_pr(vd_c1(), vd_dr()); NEXT
	OP(lea_pr) vp_lea_pr(vd_c1(), vd_dr()); NEXT
	OP(sync) vp_sync(ir >> 8); NEXT
	// doubles
	RR(add, dd)
	RR(sub, dd)
	RR(mul, dd)
	RR(div, dd)
	RR(min, dd)
	RR(max, dd)
	RR(sqrt, dd)
	RR(abs, dd)
	RR(neg, dd)
	RR(cpy, dd)
	RR(cvt, rd)
	RR(cvt, dr)
	RR(cpy, rd)
	RR(cpy, dr)
	RR(cmp, dd)
	B2(fbeq)
	B2(fbne)
	B2(fbge)
	B2(fblt)
	B2(fble)
	B2(fbgt)
	IR2(load_rcd)
	RI2(store_drc)
	DR(load_rrd)
	OP(store_drr) vp_store_drr(vd_dr(), vd_sr(), vd_sc()); NEXT
	// singles, a register the double it is of
	RR(add, ff)
	RR(sub, ff)
	RR(mul, ff)
	RR(div, ff)
	RR(min, ff)
	RR(max, ff)
	RR(sqrt, ff)
	RR(abs, ff)
	RR(neg, ff)
	RR(cvt, rf)
	RR(cvt, fr)
	RR(cvt, fd)
	RR(cvt, df)
	RR(cpy, rf)
	RR(cpy, fr)
	RR(cmp, ff)
	IR2(load_rcf)
	RI2(store_frc)
	DR(load_rrf)
	OP(store_frr) vp_store_frr(vd_dr(), vd_sr(), vd_sc()); NEXT
#ifdef VP64_SWITCH
	default:
#else
	op_bad:
#endif
	printf("Unrecognised opcode %d\n", (int)(ir & 0xff)); NEXT
#ifdef VP64_SWITCH
	}}
#endif
	return 0;
}

int vp64(uint8_t *data, int64_t *stack, int64_t *argv, void **host_calls)
{
	return vp64_validate ? vp64_run<true>(data, stack, argv, host_calls)
		: vp64_run<false>(data, stack, argv, host_calls);
}