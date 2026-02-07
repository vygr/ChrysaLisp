#include <sys/types.h>
#include <iostream>
#include <atomic>
#include <cmath>
#ifdef _WIN64
	#include <immintrin.h>
	#include <intrin.h>
#endif
#include "pii.h"

enum Opcodes {
	// Group 1: ALU (CR)
	vp64_cpy_cr_0, vp64_cpy_cr_1, vp64_cpy_cr_2, vp64_cpy_cr_3,
	vp64_add_cr_0, vp64_add_cr_1, vp64_add_cr_2, vp64_add_cr_3,
	vp64_sub_cr_0, vp64_sub_cr_1, vp64_sub_cr_2, vp64_sub_cr_3,
	vp64_cmp_cr_0, vp64_cmp_cr_1, vp64_cmp_cr_2, vp64_cmp_cr_3,
	vp64_mul_cr_0, vp64_mul_cr_1, vp64_mul_cr_2, vp64_mul_cr_3,
	vp64_and_cr_0, vp64_and_cr_1, vp64_and_cr_2, vp64_and_cr_3,
	vp64_or_cr_0, vp64_or_cr_1, vp64_or_cr_2, vp64_or_cr_3,
	vp64_xor_cr_0, vp64_xor_cr_1, vp64_xor_cr_2, vp64_xor_cr_3,

	// Group 2: ALU (RR)
	vp64_cpy_rr, vp64_add_rr, vp64_sub_rr, vp64_cmp_rr,
	vp64_mul_rr, vp64_and_rr, vp64_or_rr, vp64_xor_rr,

	// Group 3: Shift/Logic
	vp64_shl_cr, vp64_shr_cr, vp64_asr_cr,
	vp64_shl_rr, vp64_shr_rr, vp64_asr_rr,
	vp64_lnot_rr, vp64_land_rr, vp64_swp_rr, vp64_ext_rr,

	// Group 4: Division
	vp64_div_rrr, vp64_div_rrr_u,

	// Group 5: Min/Max/Abs
	vp64_min_cr_0, vp64_min_cr_1, vp64_min_cr_2, vp64_min_cr_3,
	vp64_max_cr_0, vp64_max_cr_1, vp64_max_cr_2, vp64_max_cr_3,
	vp64_min_rr, vp64_max_rr, vp64_abs_rr,

	// Group 6: Set-on-Comparison (CR)
	vp64_seq_cr_0, vp64_seq_cr_1, vp64_seq_cr_2,
	vp64_sne_cr_0, vp64_sne_cr_1, vp64_sne_cr_2,
	vp64_slt_cr_0, vp64_slt_cr_1, vp64_slt_cr_2,
	vp64_sle_cr_0, vp64_sle_cr_1, vp64_sle_cr_2,
	vp64_sgt_cr_0, vp64_sgt_cr_1, vp64_sgt_cr_2,
	vp64_sge_cr_0, vp64_sge_cr_1, vp64_sge_cr_2,

	// Group 7: Set-on-Comparison (RR)
	vp64_seq_rr, vp64_sne_rr, vp64_slt_rr,
	vp64_sle_rr, vp64_sgt_rr, vp64_sge_rr,

	// Group 11: Load/Store (Indexed I)
	vp64_cpy_ir, vp64_cpy_ir_b, vp64_cpy_ir_s, vp64_cpy_ir_i,
	vp64_cpy_ir_ub, vp64_cpy_ir_us, vp64_cpy_ir_ui, vp64_lea_i,
	vp64_cpy_ri, vp64_cpy_ri_b, vp64_cpy_ri_s, vp64_cpy_ri_i,

	// Group 12: Load/Store (Indexed D)
	vp64_cpy_rd, vp64_cpy_rd_b, vp64_cpy_rd_s, vp64_cpy_rd_i,
	vp64_cpy_dr, vp64_cpy_dr_b, vp64_cpy_dr_s, vp64_cpy_dr_i,
	vp64_cpy_dr_ub, vp64_cpy_dr_us, vp64_cpy_dr_ui, vp64_lea_d,

	// Group 16: Float Memory/Cmp
	vp64_cpy_if, vp64_cpy_fi, vp64_cpy_df, vp64_cpy_fd, vp64_cmp_f,

	// Group 8: Branch (Conditional)
	vp64_beq_0, vp64_beq_1, vp64_bne_0, vp64_bne_1,
	vp64_bge_0, vp64_bge_1, vp64_blt_0, vp64_blt_1,
	vp64_ble_0, vp64_ble_1, vp64_bgt_0, vp64_bgt_1,

	// Group 9: Branch (Float)
	vp64_fbeq_0, vp64_fbeq_1, vp64_fbne_0, vp64_fbne_1,
	vp64_fbge_0, vp64_fbge_1, vp64_fblt_0, vp64_fblt_1,
	vp64_fble_0, vp64_fble_1, vp64_fbgt_0, vp64_fbgt_1,

	// Group 10: Jump/Call
	vp64_call_0, vp64_call_1, vp64_jmp_0, vp64_jmp_1,
	vp64_call_p_0, vp64_call_p_1, vp64_jmp_p_0, vp64_jmp_p_1,
	vp64_call_r, vp64_jmp_r,
	vp64_call_i, vp64_jmp_i,

	// Group 13: PC Relative / System
	vp64_cpy_pr, vp64_lea_p, vp64_call_abi, vp64_ret, vp64_sync, vp64_brk,

	// Group 14: Float ALU
	vp64_add_ff, vp64_sub_ff, vp64_mul_ff, vp64_div_ff, vp64_min_ff,
	vp64_max_ff, vp64_sqrt_ff, vp64_abs_ff, vp64_neg_ff, vp64_cpy_ff,

	// Group 15: Float Convert/Copy
	vp64_cvt_rf, vp64_cvt_fr, vp64_cpy_rf, vp64_cpy_fr,
};

struct i128 { int64_t lo; int64_t hi; };
struct u128 { uint64_t lo; uint64_t hi; };

// generic operations
#define vp_op(op, s, d) d = d op s
#define vp_op_cr(op, c, dr) vp_op(op, c, regs[dr])
#define vp_op_rr(op, sr, dr) vp_op(op, regs[sr], regs[dr])
#define vp_op_ff(op, sr, dr) vp_op(op, fregs[sr], fregs[dr])

#define vp_cpy_cr(c, dr) regs[dr] = (c)
#define vp_add_cr(c, dr) vp_op_cr(+, c, dr)
#define vp_sub_cr(c, dr) vp_op_cr(-, c, dr)
#define vp_cmp_cr(c, dr) { compare1 = regs[dr]; compare2 = (c); }
#define vp_mul_cr(c, dr) vp_op_cr(*, c, dr)
#define vp_and_cr(c, dr) vp_op_cr(&, c, dr)
#define vp_or_cr(c, dr) vp_op_cr(|, c, dr)
#define vp_xor_cr(c, dr) vp_op_cr(^, c, dr)
#define vp_shl_cr(c, dr) vp_op_cr(<<, c, dr)
#define vp_shr_cr(c, dr) regs[dr] = (uint64_t)regs[dr] >> (c)
#define vp_asr_cr(c, dr) vp_op_cr(>>, c, dr)
#define vp_seq_cr(c, dr) vp_op_cr(==, c, dr)
#define vp_sne_cr(c, dr) vp_op_cr(!=, c, dr)
#define vp_slt_cr(c, dr) vp_op_cr(<, c, dr)
#define vp_sle_cr(c, dr) vp_op_cr(<=, c, dr)
#define vp_sgt_cr(c, dr) vp_op_cr(>, c, dr)
#define vp_sge_cr(c, dr) vp_op_cr(>=, c, dr)
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
#define vp_shl_rr(sr, dr) vp_op_rr(<<, sr, dr)
#define vp_shr_rr(sr, dr) regs[dr] = (uint64_t)regs[dr] >> regs[sr]
#define vp_asr_rr(sr, dr) vp_op_rr(>>, sr, dr)
#define vp_lnot_rr(sr, dr) regs[dr] = !regs[dr]
#define vp_land_rr(sr, dr) vp_op_rr(&&, sr, dr)
#define vp_swp_rr(sr, dr) { int64_t t = regs[dr]; regs[dr] = regs[sr]; regs[sr] = t; }
#define vp_ext_rr(sr, dr) regs[dr] = (regs[sr] >> 63)

#ifdef _WIN64
#define vp_div_rrr(sr, dr, drr) regs[dr] = _div128(regs[sr], regs[dr], regs[drr], &regs[sr])
#define vp_div_rrr_u(sr, dr, drr) regs[dr] = _udiv128((uint64_t)regs[sr], (uint64_t)regs[dr], (uint64_t)regs[drr], (uint64_t*)&regs[sr])
#else
#define vp_div_rrr(sr, dr, drr) { i128 v = {regs[dr], regs[sr]}; int64_t d = regs[drr]; regs[dr] = (__int128_t&)v / d; regs[sr] = (__int128_t&)v % d; }
#define vp_div_rrr_u(sr, dr, drr) { u128 v = {(uint64_t)regs[dr], (uint64_t)regs[sr]}; uint64_t d = (uint64_t)regs[drr]; regs[dr] = (__uint128_t&)v / d; regs[sr] = (__uint128_t&)v % d; }
#endif

#define vp_seq_rr(sr, dr) vp_op_rr(==, sr, dr)
#define vp_sne_rr(sr, dr) vp_op_rr(!=, sr, dr)
#define vp_slt_rr(sr, dr) vp_op_rr(<, sr, dr)
#define vp_sle_rr(sr, dr) vp_op_rr(<=, sr, dr)
#define vp_sgt_rr(sr, dr) vp_op_rr(>, sr, dr)
#define vp_sge_rr(sr, dr) vp_op_rr(>=, sr, dr)
#define vp_min_rr(sr, dr) if (regs[sr] < regs[dr]) regs[dr] = regs[sr]
#define vp_max_rr(sr, dr) if (regs[sr] > regs[dr]) regs[dr] = regs[sr]
#define vp_abs_rr(sr, dr) { int64_t t = regs[sr]; regs[dr] = (t < 0) ? -t : t; }

#define vp_mem_ir(sr, o, dr, t) regs[dr] = (int64_t)*(t*)(regs[sr] + o)
#define vp_mem_ri(sr, dr, o, t) *(t*)(regs[dr] + o) = (t)regs[sr]
#define vp_lea_i(sr, o, dr) regs[dr] = regs[sr] + o
#define vp_cpy_ir_b(sr, o, dr) vp_mem_ir(sr, o, dr, int8_t)
#define vp_cpy_ir_s(sr, o, dr) vp_mem_ir(sr, o, dr, int16_t)
#define vp_cpy_ir_i(sr, o, dr) vp_mem_ir(sr, o, dr, int32_t)
#define vp_cpy_ir(sr, o, dr) vp_mem_ir(sr, o, dr, int64_t)
#define vp_cpy_ir_ub(sr, o, dr) vp_mem_ir(sr, o, dr, uint8_t)
#define vp_cpy_ir_us(sr, o, dr) vp_mem_ir(sr, o, dr, uint16_t)
#define vp_cpy_ir_ui(sr, o, dr) vp_mem_ir(sr, o, dr, uint32_t)
#define vp_cpy_ri_b(sr, dr, o) vp_mem_ri(sr, dr, o, int8_t)
#define vp_cpy_ri_s(sr, dr, o) vp_mem_ri(sr, dr, o, int16_t)
#define vp_cpy_ri_i(sr, dr, o) vp_mem_ri(sr, dr, o, int32_t)
#define vp_cpy_ri(sr, dr, o) vp_mem_ri(sr, dr, o, int64_t)

#define vp_mem_dr(b, sr, dr, t) regs[dr] = (int64_t)*(t*)(regs[b] + regs[sr])
#define vp_mem_rd(sr, b, dr, t) *(t*)(regs[b] + regs[dr]) = (t)regs[sr]
#define vp_lea_d(b, sr, dr) regs[dr] = regs[b] + regs[sr]
#define vp_cpy_rd_b(sr, b, dr) vp_mem_rd(sr, b, dr, uint8_t)
#define vp_cpy_rd_s(sr, b, dr) vp_mem_rd(sr, b, dr, uint16_t)
#define vp_cpy_rd_i(sr, b, dr) vp_mem_rd(sr, b, dr, uint32_t)
#define vp_cpy_rd(sr, b, dr) vp_mem_rd(sr, b, dr, uint64_t)
#define vp_cpy_dr_b(b, sr, dr) vp_mem_dr(b, sr, dr, int8_t)
#define vp_cpy_dr_s(b, sr, dr) vp_mem_dr(b, sr, dr, int16_t)
#define vp_cpy_dr_i(b, sr, dr) vp_mem_dr(b, sr, dr, int32_t)
#define vp_cpy_dr(b, sr, dr) vp_mem_dr(b, sr, dr, int64_t)
#define vp_cpy_dr_ub(b, sr, dr) vp_mem_dr(b, sr, dr, uint8_t)
#define vp_cpy_dr_us(b, sr, dr) vp_mem_dr(b, sr, dr, uint16_t)
#define vp_cpy_dr_ui(b, sr, dr) vp_mem_dr(b, sr, dr, uint32_t)

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
#define vp_call_i(br, o) { int64_t b = regs[br], _o = (o); vp_push_pc(); pc = vp_pc_ind(b + _o); }
#define vp_jmp_i(br, o) { int64_t b = regs[br], _o = (o); pc = vp_pc_ind(b + _o); }
#define vp_cpy_pr(o, dr) { int64_t _o = (o); regs[dr] = *(int64_t*)vp_pc_rel(_o); }
#define vp_lea_p(o, dr) { int64_t _o = (o); regs[dr] = (int64_t)vp_pc_rel(_o); }
#define vp_ret() vp_pop_pc()

#define vp_sync(c) sync.store((c), std::memory_order_seq_cst)
#define vp_brk(c) std::cout << "brk " << (int)(c) << std::endl

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

#define vp_add_ff(sr, dr) vp_op_ff(+, sr, dr)
#define vp_sub_ff(sr, dr) vp_op_ff(-, sr, dr)
#define vp_mul_ff(sr, dr) vp_op_ff(*, sr, dr)
#define vp_div_ff(sr, dr) vp_op_ff(/, sr, dr)
#define vp_min_ff(sr, dr) fregs[dr] = std::fmin(fregs[dr], fregs[sr])
#define vp_max_ff(sr, dr) fregs[dr] = std::fmax(fregs[dr], fregs[sr])
#define vp_sqrt_ff(sr, dr) fregs[dr] = std::sqrt(fregs[sr])
#define vp_abs_ff(sr, dr) fregs[dr] = std::abs(fregs[sr])
#define vp_neg_ff(sr, dr) fregs[dr] = -fregs[sr]

#define vp_fbranch(c, o) { int64_t _o = (o); if (c) pc = vp_pc_rel(_o); }
#define vp_fbeq(o) vp_fbranch(compare_f1 == compare_f2, o)
#define vp_fbne(o) vp_fbranch(compare_f1 != compare_f2, o)
#define vp_fbge(o) vp_fbranch(compare_f2 >= compare_f1, o)
#define vp_fblt(o) vp_fbranch(compare_f2 < compare_f1, o)
#define vp_fble(o) vp_fbranch(compare_f2 <= compare_f1, o)
#define vp_fbgt(o) vp_fbranch(compare_f2 > compare_f1, o)

#define vp_mem_if(br, o, dr, t) fregs[dr] = (double)*(t*)(regs[br] + o)
#define vp_mem_fi(sr, br, o, t) *(t*)(regs[br] + o) = (t)fregs[sr]
#define vp_mem_df(br, ir, dr, t) fregs[dr] = (double)*(t*)(regs[br] + regs[ir])
#define vp_mem_fd(sr, br, ir, t) *(t*)(regs[br] + regs[ir]) = (t)fregs[sr]

#define vp_cpy_if(br, o, dr) vp_mem_if(br, o, dr, double)
#define vp_cpy_fi(sr, br, o) vp_mem_fi(sr, br, o, double)
#define vp_cpy_df(br, ir, dr) vp_mem_df(br, ir, dr, double)
#define vp_cpy_fd(sr, br, ir) vp_mem_fd(sr, br, ir, double)

#define vp_cvt_rf(sr, dr) fregs[dr] = (double)regs[sr]
#define vp_cvt_fr(sr, dr) regs[dr] = (int64_t)fregs[sr]
#define vp_cpy_ff(sr, dr) fregs[dr] = fregs[sr]
#define vp_cpy_rf(sr, dr) *(int64_t*)&fregs[dr] = regs[sr]
#define vp_cpy_fr(sr, dr) regs[dr] = *(int64_t*)&fregs[sr]
#define vp_cmp_f(sr, dr) { compare_f1 = fregs[sr]; compare_f2 = fregs[dr]; }

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

#define VP_CR_OP(op) \
	case vp64_##op##_cr_0: vp_##op##_cr(vd_c0(), vd_dr()); break; \
	case vp64_##op##_cr_1: vp_##op##_cr(vd_c1(), vd_dr()); break; \
	case vp64_##op##_cr_2: vp_##op##_cr(vd_c2(), vd_dr()); break; \
	case vp64_##op##_cr_3: vp_##op##_cr(vd_c3(), vd_dr()); break;
#define VP_CR_OP3(op) \
	case vp64_##op##_cr_0: vp_##op##_cr(vd_c0(), vd_dr()); break; \
	case vp64_##op##_cr_1: vp_##op##_cr(vd_c1(), vd_dr()); break; \
	case vp64_##op##_cr_2: vp_##op##_cr(vd_c2(), vd_dr()); break;
#define VP_OP_RR(op, type) \
	case vp64_##op##_##type: vp_##op##_##type(vd_sr(), vd_dr()); break;
#define VP_B_OP(op) \
	case vp64_##op##_0: vp_##op(vd_o0()); break; \
	case vp64_##op##_1: vp_##op(vd_o1()); break;
#define VP_IR_OP(op) \
	case vp64_##op##_ir: vp_##op##_ir(vd_sr(), vd_i(), vd_dr()); break; \
	case vp64_##op##_ir_b: vp_##op##_ir_b(vd_sr(), vd_i(), vd_dr()); break; \
	case vp64_##op##_ir_s: vp_##op##_ir_s(vd_sr(), vd_i(), vd_dr()); break; \
	case vp64_##op##_ir_i: vp_##op##_ir_i(vd_sr(), vd_i(), vd_dr()); break; \
	case vp64_##op##_ir_ub: vp_##op##_ir_ub(vd_sr(), vd_i(), vd_dr()); break; \
	case vp64_##op##_ir_us: vp_##op##_ir_us(vd_sr(), vd_i(), vd_dr()); break; \
	case vp64_##op##_ir_ui: vp_##op##_ir_ui(vd_sr(), vd_i(), vd_dr()); break;
#define VP_RI_OP(op) \
	case vp64_##op##_ri: vp_##op##_ri(vd_sr(), vd_dr(), vd_i()); break; \
	case vp64_##op##_ri_b: vp_##op##_ri_b(vd_sr(), vd_dr(), vd_i()); break; \
	case vp64_##op##_ri_s: vp_##op##_ri_s(vd_sr(), vd_dr(), vd_i()); break; \
	case vp64_##op##_ri_i: vp_##op##_ri_i(vd_sr(), vd_dr(), vd_i()); break;
#define VP_RD_OP(op) \
	case vp64_##op##_rd: vp_##op##_rd(vd_sc(), vd_dr(), vd_sr()); break; \
	case vp64_##op##_rd_b: vp_##op##_rd_b(vd_sc(), vd_dr(), vd_sr()); break; \
	case vp64_##op##_rd_s: vp_##op##_rd_s(vd_sc(), vd_dr(), vd_sr()); break; \
	case vp64_##op##_rd_i: vp_##op##_rd_i(vd_sc(), vd_dr(), vd_sr()); break;
#define VP_DR_OP(op) \
	case vp64_##op##_dr: vp_##op##_dr(vd_sr(), vd_sc(), vd_dr()); break; \
	case vp64_##op##_dr_b: vp_##op##_dr_b(vd_sr(), vd_sc(), vd_dr()); break; \
	case vp64_##op##_dr_s: vp_##op##_dr_s(vd_sr(), vd_sc(), vd_dr()); break; \
	case vp64_##op##_dr_i: vp_##op##_dr_i(vd_sr(), vd_sc(), vd_dr()); break; \
	case vp64_##op##_dr_ub: vp_##op##_dr_ub(vd_sr(), vd_sc(), vd_dr()); break; \
	case vp64_##op##_dr_us: vp_##op##_dr_us(vd_sr(), vd_sc(), vd_dr()); break; \
	case vp64_##op##_dr_ui: vp_##op##_dr_ui(vd_sr(), vd_sc(), vd_dr()); break;

#define VP_CR_ALL \
	VP_CR_OP(cpy) \
	VP_CR_OP(add) \
	VP_CR_OP(sub) \
	VP_CR_OP(cmp) \
	VP_CR_OP(mul) \
	VP_CR_OP(and) \
	VP_CR_OP(or) \
	VP_CR_OP(xor)

#define VP_RR_ALL \
	VP_OP_RR(cpy, rr) \
	VP_OP_RR(add, rr) \
	VP_OP_RR(sub, rr) \
	VP_OP_RR(cmp, rr) \
	VP_OP_RR(mul, rr) \
	VP_OP_RR(and, rr) \
	VP_OP_RR(or, rr) \
	VP_OP_RR(xor, rr)

#define VP_SHIFT_ALL \
	case vp64_shl_cr: vp_shl_cr(vd_sc(), vd_dr()); break; \
	case vp64_shr_cr: vp_shr_cr(vd_sc(), vd_dr()); break; \
	case vp64_asr_cr: vp_asr_cr(vd_sc(), vd_dr()); break; \
	VP_OP_RR(shl, rr) \
	VP_OP_RR(shr, rr) \
	VP_OP_RR(asr, rr) \
	VP_OP_RR(lnot, rr) \
	VP_OP_RR(land, rr) \
	VP_OP_RR(swp, rr) \
	VP_OP_RR(ext, rr)

#define VP_DIV_ALL \
	case vp64_div_rrr: vp_div_rrr(vd_sr(), vd_dr(), vd_sc()); break; \
	case vp64_div_rrr_u: vp_div_rrr_u(vd_sr(), vd_dr(), vd_sc()); break;

#define VP_M_ALL \
	case vp64_min_cr_0: vp_min_cr(vd_c0(), vd_dr()); break; \
	case vp64_min_cr_1: vp_min_cr(vd_c1(), vd_dr()); break; \
	case vp64_min_cr_2: vp_min_cr(vd_c2(), vd_dr()); break; \
	case vp64_min_cr_3: vp_min_cr(vd_c3(), vd_dr()); break; \
	case vp64_max_cr_0: vp_max_cr(vd_c0(), vd_dr()); break; \
	case vp64_max_cr_1: vp_max_cr(vd_c1(), vd_dr()); break; \
	case vp64_max_cr_2: vp_max_cr(vd_c2(), vd_dr()); break; \
	case vp64_max_cr_3: vp_max_cr(vd_c3(), vd_dr()); break; \
	VP_OP_RR(min, rr) \
	VP_OP_RR(max, rr) \
	VP_OP_RR(abs, rr)

#define VP_CMP_ALL \
	VP_CR_OP3(seq) \
	VP_CR_OP3(sne) \
	VP_CR_OP3(slt) \
	VP_CR_OP3(sle) \
	VP_CR_OP3(sgt) \
	VP_CR_OP3(sge) \
	VP_OP_RR(seq, rr) \
	VP_OP_RR(sne, rr) \
	VP_OP_RR(slt, rr) \
	VP_OP_RR(sle, rr) \
	VP_OP_RR(sgt, rr) \
	VP_OP_RR(sge, rr)

#define VP_IR_ALL \
	VP_IR_OP(cpy) \
	case vp64_lea_i: vp_lea_i(vd_sr(), vd_i(), vd_dr()); break;

#define VP_RI_ALL \
	VP_RI_OP(cpy)

#define VP_RD_ALL \
	VP_RD_OP(cpy)

#define VP_DR_ALL \
	VP_DR_OP(cpy) \
	case vp64_lea_d: vp_lea_d(vd_sr(), vd_sc(), vd_dr()); break;

#define VP_F_MEM_ALL \
	case vp64_cpy_if: vp_cpy_if(vd_sr(), vd_i(), vd_dr()); break; \
	case vp64_cpy_fi: vp_cpy_fi(vd_sr(), vd_dr(), vd_i()); break; \
	case vp64_cpy_df: vp_cpy_df(vd_sr(), vd_sc(), vd_dr()); break; \
	case vp64_cpy_fd: vp_cpy_fd(vd_dr(), vd_sr(), vd_sc()); break; \
	case vp64_cmp_f: vp_cmp_f(vd_sr(), vd_dr()); break;

#define VP_B_ALL \
	VP_B_OP(beq) \
	VP_B_OP(bne) \
	VP_B_OP(bge) \
	VP_B_OP(blt) \
	VP_B_OP(ble) \
	VP_B_OP(bgt)

#define VP_F_BRANCH_ALL \
	VP_B_OP(fbeq) \
	VP_B_OP(fbne) \
	VP_B_OP(fbge) \
	VP_B_OP(fblt) \
	VP_B_OP(fble) \
	VP_B_OP(fbgt)

#define VP_JC_ALL \
	VP_B_OP(call) \
	VP_B_OP(jmp) \
	VP_B_OP(call_p) \
	VP_B_OP(jmp_p) \
	case vp64_call_r: vp_call_r(vd_dr()); break; \
	case vp64_jmp_r: vp_jmp_r(vd_dr()); break; \
	case vp64_call_i: vp_call_i(vd_dr(), vd_c1()); break; \
	case vp64_jmp_i: vp_jmp_i(vd_dr(), vd_c1()); break;

#define VP_SYS_ALL \
	case vp64_cpy_pr: vp_cpy_pr(vd_c1(), vd_dr()); break; \
	case vp64_lea_p: vp_lea_p(vd_c1(), vd_dr()); break; \
	case vp64_call_abi: vp_call_abi(vd_sr(), vd_dr(), vd_sc()); break; \
	case vp64_ret: vp_ret(); break; \
	case vp64_sync: vp_sync(ir >> 8); break; \
	case vp64_brk: vp_brk((ir >> 8) & 0xff); break;

#define VP_F_ALU_ALL \
	VP_OP_RR(add, ff) \
	VP_OP_RR(sub, ff) \
	VP_OP_RR(mul, ff) \
	VP_OP_RR(div, ff) \
	VP_OP_RR(min, ff) \
	VP_OP_RR(max, ff) \
	VP_OP_RR(sqrt, ff) \
	VP_OP_RR(abs, ff) \
	VP_OP_RR(neg, ff) \
	VP_OP_RR(cpy, ff)

#define VP_F_CVT_ALL \
	VP_OP_RR(cvt, rf) \
	VP_OP_RR(cvt, fr) \
	VP_OP_RR(cpy, rf) \
	VP_OP_RR(cpy, fr)

int vp64(uint8_t* data, int64_t *stack, int64_t* argv, int64_t* host_os_funcs, int64_t* host_gui_funcs, int64_t* host_audio_funcs)
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

	fn_header* pHeader = (fn_header*)data;
	pc = (int16_t*)((uint8_t*)data + pHeader->entry);

	regs[0] = (int64_t)argv;
	regs[1] = (int64_t)host_os_funcs;
	regs[2] = (int64_t)host_gui_funcs;
	regs[3] = (int64_t)host_audio_funcs;
	regs[15] = (int64_t)stack;

	for(;;)
	{
		ir = *pc++;
		switch (ir & 0xff)
		{
			VP_CR_ALL VP_RR_ALL VP_SHIFT_ALL VP_DIV_ALL VP_M_ALL VP_CMP_ALL
			VP_IR_ALL VP_RI_ALL VP_RD_ALL VP_DR_ALL VP_F_MEM_ALL VP_B_ALL
			VP_F_BRANCH_ALL VP_JC_ALL VP_SYS_ALL VP_F_ALU_ALL VP_F_CVT_ALL
			default: std::cout << "Unrecognised opcode " << (ir & 0xff) << " " << std::endl; break;
		}
	}
	return 0;
}
