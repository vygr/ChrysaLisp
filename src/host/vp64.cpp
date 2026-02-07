#include <sys/types.h>
#include <iostream>
#include <atomic>
#include <cmath>
#ifdef _WIN64
	#include <immintrin.h>
#endif
#include "pii.h"

enum Opcodes {
	VP64_CPY_CR_0, VP64_CPY_CR_1, VP64_CPY_CR_2, VP64_CPY_CR_3, VP64_ADD_CR_0, VP64_ADD_CR_1, VP64_ADD_CR_2, VP64_ADD_CR_3,
	VP64_SUB_CR_0, VP64_SUB_CR_1, VP64_SUB_CR_2, VP64_SUB_CR_3, VP64_CMP_CR_0, VP64_CMP_CR_1, VP64_CMP_CR_2, VP64_CMP_CR_3,
	VP64_MUL_CR_0, VP64_MUL_CR_1, VP64_MUL_CR_2, VP64_MUL_CR_3, VP64_AND_CR_0, VP64_AND_CR_1, VP64_AND_CR_2, VP64_AND_CR_3,
	VP64_OR_CR_0, VP64_OR_CR_1, VP64_OR_CR_2, VP64_OR_CR_3, VP64_XOR_CR_0, VP64_XOR_CR_1, VP64_XOR_CR_2, VP64_XOR_CR_3,
	VP64_SHL_CR, VP64_SHR_CR, VP64_ASR_CR,
	VP64_CPY_RR, VP64_ADD_RR, VP64_SUB_RR, VP64_CMP_RR, VP64_MUL_RR, VP64_AND_RR, VP64_OR_RR, VP64_XOR_RR,
	VP64_SHL_RR, VP64_SHR_RR, VP64_ASR_RR, VP64_LNOT_RR, VP64_LAND_RR, VP64_SWP_RR, VP64_EXT_RR,
	VP64_DIV_RRR, VP64_DIV_RRR_U,
	VP64_SEQ_CR_0, VP64_SEQ_CR_1, VP64_SEQ_CR_2, VP64_SNE_CR_0, VP64_SNE_CR_1, VP64_SNE_CR_2,
	VP64_SLT_CR_0, VP64_SLT_CR_1, VP64_SLT_CR_2, VP64_SLE_CR_0, VP64_SLE_CR_1, VP64_SLE_CR_2,
	VP64_SGT_CR_0, VP64_SGT_CR_1, VP64_SGT_CR_2, VP64_SGE_CR_0, VP64_SGE_CR_1, VP64_SGE_CR_2,
	VP64_SEQ_RR, VP64_SNE_RR, VP64_SLT_RR, VP64_SLE_RR, VP64_SGT_RR, VP64_SGE_RR,
	VP64_BEQ_0, VP64_BEQ_1, VP64_BNE_0, VP64_BNE_1, VP64_BGE_0, VP64_BGE_1,
	VP64_BLT_0, VP64_BLT_1, VP64_BLE_0, VP64_BLE_1, VP64_BGT_0, VP64_BGT_1,
	VP64_CPY_IR_0, VP64_CPY_IR_B_0, VP64_CPY_IR_S_0, VP64_CPY_IR_I_0, VP64_CPY_IR_UB_0, VP64_CPY_IR_US_0, VP64_CPY_IR_UI_0, VP64_LEA_I_0,
	VP64_CPY_RI_0, VP64_CPY_RI_B_0, VP64_CPY_RI_S_0, VP64_CPY_RI_I_0,
	VP64_CPY_RD, VP64_CPY_RD_B, VP64_CPY_RD_S, VP64_CPY_RD_I,
	VP64_CPY_DR, VP64_CPY_DR_B, VP64_CPY_DR_S, VP64_CPY_DR_I, VP64_CPY_DR_UB, VP64_CPY_DR_US, VP64_CPY_DR_UI, VP64_LEA_D,
	VP64_CALL_R, VP64_JMP_R, VP64_CALL_I, VP64_JMP_I, VP64_CPY_PR, VP64_LEA_P,
	VP64_CALL_0, VP64_CALL_1, VP64_JMP_0, VP64_JMP_1, VP64_CALL_P_0, VP64_CALL_P_1, VP64_JMP_P_0, VP64_JMP_P_1,
	VP64_CALL_ABI, VP64_RET, VP64_SYNC, VP64_BRK,
	VP64_MIN_CR_0, VP64_MIN_CR_1, VP64_MIN_CR_2, VP64_MIN_CR_3, VP64_MAX_CR_0, VP64_MAX_CR_1, VP64_MAX_CR_2, VP64_MAX_CR_3,
	VP64_MIN_RR, VP64_MAX_RR, VP64_ABS_RR,
	VP64_CMP_F, VP64_CPY_FF, VP64_CPY_RF, VP64_CPY_FR, VP64_CVT_RF, VP64_CVT_FR,
	VP64_ADD_FF, VP64_SUB_FF, VP64_MUL_FF, VP64_DIV_FF, VP64_MIN_FF, VP64_MAX_FF, VP64_SQRT_FF,
	VP64_FBEQ_0, VP64_FBEQ_1, VP64_FBNE_0, VP64_FBNE_1, VP64_FBGE_0, VP64_FBGE_1,
	VP64_FBLT_0, VP64_FBLT_1, VP64_FBLE_0, VP64_FBLE_1, VP64_FBGT_0, VP64_FBGT_1,
	VP64_CPY_IF, VP64_CPY_FI, VP64_ABS_FF, VP64_NEG_FF, VP64_CPY_DF, VP64_CPY_FD,
};

#include <atomic>
#ifdef _WIN64
#include <intrin.h>
#endif

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
#define vp_abs_rr(sr, dr) { int64_t _sr = regs[sr]; regs[dr] = (_sr < 0) ? -_sr : _sr; }

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

#define vp_branch(c, o) { int64_t _o = (o); if (c) pc = (int16_t*)((char*)pc + _o); }
#define vp_beq(o) vp_branch(compare1 == compare2, o)
#define vp_bne(o) vp_branch(compare1 != compare2, o)
#define vp_bge(o) vp_branch(compare1 >= compare2, o)
#define vp_blt(o) vp_branch(compare1 < compare2, o)
#define vp_ble(o) vp_branch(compare1 <= compare2, o)
#define vp_bgt(o) vp_branch(compare1 > compare2, o)
#define vp_call(o) { int64_t _o = (o); regs[15] -= 8; *(int16_t**)regs[15] = pc; pc = (int16_t*)((char*)pc + _o); }
#define vp_jmp(o) { int64_t _o = (o); pc = (int16_t*)((char*)pc + _o); }
#define vp_call_p(o) { int64_t _o = (o); regs[15] -= 8; *(int16_t**)regs[15] = pc; pc = *(int16_t**)((char*)pc + _o); }
#define vp_jmp_p(o) { int64_t _o = (o); pc = *(int16_t**)((char*)pc + _o); }
#define vp_call_r(dr) { regs[15] -= 8; *(int16_t**)regs[15] = pc; pc = (int16_t*)regs[dr]; }
#define vp_jmp_r(dr) pc = (int16_t*)regs[dr]
#define vp_call_i(br, o) { int64_t b = regs[br], _o = (o); regs[15] -= 8; *(int16_t**)regs[15] = pc; pc = *(int16_t**)(b + _o); }
#define vp_jmp_i(br, o) { int64_t b = regs[br], _o = (o); pc = *(int16_t**)(b + _o); }
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
#define vp_cpy_pr(o, dr) { int64_t _o = (o); regs[dr] = *(int64_t*)((char*)pc + _o); }
#define vp_lea_p(o, dr) { int64_t _o = (o); regs[dr] = (int64_t)((char*)pc + _o); }
#define vp_ret() { pc = *(int16_t**)regs[15]; regs[15] += 8; }
#define vp_sync(c) sync.store((c), std::memory_order_seq_cst)
#define vp_brk(c) std::cout << "brk " << (int)(c) << std::endl

#define vp_add_ff(sr, dr) vp_op_ff(+, sr, dr)
#define vp_sub_ff(sr, dr) vp_op_ff(-, sr, dr)
#define vp_mul_ff(sr, dr) vp_op_ff(*, sr, dr)
#define vp_div_ff(sr, dr) vp_op_ff(/, sr, dr)
#define vp_min_ff(sr, dr) fregs[dr] = std::fmin(fregs[dr], fregs[sr])
#define vp_max_ff(sr, dr) fregs[dr] = std::fmax(fregs[dr], fregs[sr])
#define vp_sqrt_ff(sr, dr) fregs[dr] = std::sqrt(fregs[sr])
#define vp_abs_ff(sr, dr) fregs[dr] = std::abs(fregs[sr])
#define vp_neg_ff(sr, dr) fregs[dr] = -fregs[sr]
#define vp_fbranch(c, o) { int64_t _o = (o); if (c) pc = (int16_t*)((char*)pc + _o); }
#define vp_fbeq(o) vp_fbranch(compare_f1 == compare_f2, o)
#define vp_fbne(o) vp_fbranch(compare_f1 != compare_f2, o)
#define vp_fbge(o) vp_fbranch(compare_f2 >= compare_f1, o)
#define vp_fblt(o) vp_fbranch(compare_f2 < compare_f1, o)
#define vp_fble(o) vp_fbranch(compare_f2 <= compare_f1, o)
#define vp_fbgt(o) vp_fbranch(compare_f2 > compare_f1, o)
#define vp_cpy_if(br, o, dr) fregs[dr] = *(double*)(regs[br] + o)
#define vp_cpy_fi(sr, br, o) *(double*)(regs[br] + o) = fregs[sr]
#define vp_cpy_df(br, ir, dr) fregs[dr] = *(double*)(regs[br] + regs[ir])
#define vp_cpy_fd(sr, br, ir) *(double*)(regs[br] + regs[ir]) = fregs[sr]
#define vp_cvt_rf(sr, dr) fregs[dr] = (double)regs[sr]
#define vp_cvt_fr(sr, dr) regs[dr] = (int64_t)fregs[sr]
#define vp_cpy_ff(sr, dr) fregs[dr] = fregs[sr]
#define vp_cpy_rf(sr, dr) *(int64_t*)&fregs[dr] = regs[sr]
#define vp_cpy_fr(sr, dr) regs[dr] = *(int64_t*)&fregs[sr]
#define vp_cmp_f(sr, dr) { compare_f1 = fregs[sr]; compare_f2 = fregs[dr]; }

#define vp_dr() ((ir >> 8) & 0xf)
#define vp_sr() ((ir >> 12) & 0xf)
#define vp_c0() ((int64_t)ir >> 12)
#define vp_c1() (vp_sr() | (vp_i() << 4))
#define vp_c2() ([&]{ uint64_t o1 = vp_sc() << 4; int64_t o2 = vp_i() << 20; return (int64_t)(vp_sr() | o1 | o2); }())
#define vp_c3() ([&]{ uint64_t o0 = vp_sc(); uint64_t o1 = vp_sc() << 16; uint64_t o2 = vp_sc() << 32; int64_t o3 = vp_i() << 48; return (int64_t)(o0 | o1 | o2 | o3); }())
#define vp_sc() ((uint64_t)*(uint16_t*)pc++)
#define vp_i() ((int64_t)*pc++)
#define vp_o0() ((int8_t)(ir >> 8))
#define vp_o1() (((ir >> 8) & 0xff) + ((int64_t)*pc++ << 8))
#define vp_offset() vp_o1()

#define VP_CR_OP(op, lop) \
	case VP64_##op##_CR_0: vp_##lop##_cr(vp_c0(), vp_dr()); break; \
	case VP64_##op##_CR_1: vp_##lop##_cr(vp_c1(), vp_dr()); break; \
	case VP64_##op##_CR_2: vp_##lop##_cr(vp_c2(), vp_dr()); break; \
	case VP64_##op##_CR_3: vp_##lop##_cr(vp_c3(), vp_dr()); break;
#define VP_CR_OP3(op, lop) \
	case VP64_##op##_CR_0: vp_##lop##_cr(vp_c0(), vp_dr()); break; \
	case VP64_##op##_CR_1: vp_##lop##_cr(vp_c1(), vp_dr()); break; \
	case VP64_##op##_CR_2: vp_##lop##_cr(vp_c2(), vp_dr()); break;
#define VP_OP_RR(op, lop, type, ltype) case VP64_##op##_##type: vp_##lop##_##ltype(vp_sr(), vp_dr()); break;
#define VP_B_OP(op, lop) \
	case VP64_##op##_0: vp_##lop(vp_o0()); break; \
	case VP64_##op##_1: vp_##lop(vp_o1()); break;
#define VP_IR_OP(op, lop) \
	case VP64_##op##_IR_0: vp_##lop##_ir(vp_sr(), vp_i(), vp_dr()); break; \
	case VP64_##op##_IR_B_0: vp_##lop##_ir_b(vp_sr(), vp_i(), vp_dr()); break; \
	case VP64_##op##_IR_S_0: vp_##lop##_ir_s(vp_sr(), vp_i(), vp_dr()); break; \
	case VP64_##op##_IR_I_0: vp_##lop##_ir_i(vp_sr(), vp_i(), vp_dr()); break; \
	case VP64_##op##_IR_UB_0: vp_##lop##_ir_ub(vp_sr(), vp_i(), vp_dr()); break; \
	case VP64_##op##_IR_US_0: vp_##lop##_ir_us(vp_sr(), vp_i(), vp_dr()); break; \
	case VP64_##op##_IR_UI_0: vp_##lop##_ir_ui(vp_sr(), vp_i(), vp_dr()); break;
#define VP_RI_OP(op, lop) \
	case VP64_##op##_RI_0: vp_##lop##_ri(vp_sr(), vp_dr(), vp_i()); break; \
	case VP64_##op##_RI_B_0: vp_##lop##_ri_b(vp_sr(), vp_dr(), vp_i()); break; \
	case VP64_##op##_RI_S_0: vp_##lop##_ri_s(vp_sr(), vp_dr(), vp_i()); break; \
	case VP64_##op##_RI_I_0: vp_##lop##_ri_i(vp_sr(), vp_dr(), vp_i()); break;
#define VP_RD_OP(op, lop) \
	case VP64_##op##_RD: vp_##lop##_rd(vp_sc(), vp_dr(), vp_sr()); break; \
	case VP64_##op##_RD_B: vp_##lop##_rd_b(vp_sc(), vp_dr(), vp_sr()); break; \
	case VP64_##op##_RD_S: vp_##lop##_rd_s(vp_sc(), vp_dr(), vp_sr()); break; \
	case VP64_##op##_RD_I: vp_##lop##_rd_i(vp_sc(), vp_dr(), vp_sr()); break;
#define VP_DR_OP(op, lop) \
	case VP64_##op##_DR: vp_##lop##_dr(vp_sr(), vp_sc(), vp_dr()); break; \
	case VP64_##op##_DR_B: vp_##lop##_dr_b(vp_sr(), vp_sc(), vp_dr()); break; \
	case VP64_##op##_DR_S: vp_##lop##_dr_s(vp_sr(), vp_sc(), vp_dr()); break; \
	case VP64_##op##_DR_I: vp_##lop##_dr_i(vp_sr(), vp_sc(), vp_dr()); break; \
	case VP64_##op##_DR_UB: vp_##lop##_dr_ub(vp_sr(), vp_sc(), vp_dr()); break; \
	case VP64_##op##_DR_US: vp_##lop##_dr_us(vp_sr(), vp_sc(), vp_dr()); break; \
	case VP64_##op##_DR_UI: vp_##lop##_dr_ui(vp_sr(), vp_sc(), vp_dr()); break;

#define VP_CR_ALL VP_CR_OP(CPY, cpy) VP_CR_OP(ADD, add) VP_CR_OP(SUB, sub) VP_CR_OP(CMP, cmp) VP_CR_OP(MUL, mul) VP_CR_OP(AND, and) VP_CR_OP(OR, or) VP_CR_OP(XOR, xor)
#define VP_RR_ALL VP_OP_RR(CPY, cpy, RR, rr) VP_OP_RR(ADD, add, RR, rr) VP_OP_RR(SUB, sub, RR, rr) VP_OP_RR(CMP, cmp, RR, rr) VP_OP_RR(MUL, mul, RR, rr) VP_OP_RR(AND, and, RR, rr) VP_OP_RR(OR, or, RR, rr) VP_OP_RR(XOR, xor, RR, rr) VP_OP_RR(SHL, shl, RR, rr) VP_OP_RR(SHR, shr, RR, rr) VP_OP_RR(ASR, asr, RR, rr) VP_OP_RR(LNOT, lnot, RR, rr) VP_OP_RR(LAND, land, RR, rr) VP_OP_RR(SWP, swp, RR, rr) VP_OP_RR(EXT, ext, RR, rr)
#define VP_CMP_ALL VP_CR_OP3(SEQ, seq) VP_CR_OP3(SNE, sne) VP_CR_OP3(SLT, slt) VP_CR_OP3(SLE, sle) VP_CR_OP3(SGT, sgt) VP_CR_OP3(SGE, sge) VP_OP_RR(SEQ, seq, RR, rr) VP_OP_RR(SNE, sne, RR, rr) VP_OP_RR(SLT, slt, RR, rr) VP_OP_RR(SLE, sle, RR, rr) VP_OP_RR(SGT, sgt, RR, rr) VP_OP_RR(SGE, sge, RR, rr)
#define VP_B_ALL VP_B_OP(BEQ, beq) VP_B_OP(BNE, bne) VP_B_OP(BGE, bge) VP_B_OP(BLT, blt) VP_B_OP(BLE, ble) VP_B_OP(BGT, bgt)
#define VP_F_ALL VP_OP_RR(CPY, cpy, FF, ff) VP_OP_RR(CPY, cpy, RF, rf) VP_OP_RR(CPY, cpy, FR, fr) VP_OP_RR(CVT, cvt, RF, rf) VP_OP_RR(CVT, cvt, FR, fr) VP_OP_RR(ADD, add, FF, ff) VP_OP_RR(SUB, sub, FF, ff) VP_OP_RR(MUL, mul, FF, ff) VP_OP_RR(DIV, div, FF, ff) VP_OP_RR(MIN, min, FF, ff) VP_OP_RR(MAX, max, FF, ff) VP_OP_RR(SQRT, sqrt, FF, ff) VP_B_OP(FBEQ, fbeq) VP_B_OP(FBNE, fbne) VP_B_OP(FBGE, fbge) VP_B_OP(FBLT, fblt) VP_B_OP(FBLE, fble) VP_B_OP(FBGT, fbgt) VP_OP_RR(ABS, abs, FF, ff) VP_OP_RR(NEG, neg, FF, ff)
#define VP_IR_ALL VP_IR_OP(CPY, cpy) case VP64_LEA_I_0: vp_lea_i(vp_sr(), vp_i(), vp_dr()); break;
#define VP_RI_ALL VP_RI_OP(CPY, cpy)
#define VP_RD_ALL VP_RD_OP(CPY, cpy)
#define VP_DR_ALL VP_DR_OP(CPY, cpy) case VP64_LEA_D: vp_lea_d(vp_sr(), vp_sc(), vp_dr()); break;
#define VP_BC_ALL VP_B_OP(CALL, call) VP_B_OP(JMP, jmp) VP_B_OP(CALL_P, call_p) VP_B_OP(JMP_P, jmp_p)
#define VP_M_ALL case VP64_MIN_CR_0: vp_min_cr(vp_c0(), vp_dr()); break; case VP64_MIN_CR_1: vp_min_cr(vp_c1(), vp_dr()); break; case VP64_MIN_CR_2: vp_min_cr(vp_c2(), vp_dr()); break; case VP64_MIN_CR_3: vp_min_cr(vp_c3(), vp_dr()); break; case VP64_MAX_CR_0: vp_max_cr(vp_c0(), vp_dr()); break; case VP64_MAX_CR_1: vp_max_cr(vp_c1(), vp_dr()); break; case VP64_MAX_CR_2: vp_max_cr(vp_c2(), vp_dr()); break; case VP64_MAX_CR_3: vp_max_cr(vp_c3(), vp_dr()); break; VP_OP_RR(MIN, min, RR, rr) VP_OP_RR(MAX, max, RR, rr) VP_OP_RR(ABS, abs, RR, rr)

int vp64(uint8_t* data, int64_t *stack, int64_t* argv, int64_t* host_os_funcs, int64_t* host_gui_funcs, int64_t* host_audio_funcs) {
	int64_t regs[16]; double fregs[16]; int16_t* pc; int64_t ir; int64_t compare1 = 0, compare2 = 0; double compare_f1 = 0.0, compare_f2 = 0.0; std::atomic<int> sync;
	fn_header* pHeader = (fn_header*)((uint8_t*)data); pc = (int16_t*)((uint8_t*)data + pHeader->entry);
	regs[0] = (int64_t)argv; regs[1] = (int64_t)host_os_funcs; regs[2] = (int64_t)host_gui_funcs; regs[3] = (int64_t)host_audio_funcs; regs[15] = (int64_t)stack;
	for(;;) {
		ir = *pc++;
		switch (ir & 0xff) {
			VP_CR_ALL
			case VP64_SHL_CR: vp_shl_cr(vp_sc(), vp_dr()); break;
			case VP64_SHR_CR: vp_shr_cr(vp_sc(), vp_dr()); break;
			case VP64_ASR_CR: vp_asr_cr(vp_sc(), vp_dr()); break;
			VP_RR_ALL
			case VP64_DIV_RRR: vp_div_rrr(vp_sr(), vp_dr(), vp_sc()); break;
			case VP64_DIV_RRR_U: vp_div_rrr_u(vp_sr(), vp_dr(), vp_sc()); break;
			VP_CMP_ALL VP_B_ALL VP_IR_ALL VP_RI_ALL VP_RD_ALL VP_DR_ALL VP_BC_ALL
			case VP64_CALL_ABI: vp_call_abi(vp_sr(), vp_dr(), vp_sc()); break;
			case VP64_CALL_R: vp_call_r(vp_dr()); break;
			case VP64_JMP_R: vp_jmp_r(vp_dr()); break;
			case VP64_CALL_I: vp_call_i(vp_dr(), vp_c1()); break;
			case VP64_JMP_I: vp_jmp_i(vp_dr(), vp_c1()); break;
			case VP64_CPY_PR: vp_cpy_pr(vp_c1(), vp_dr()); break;
			case VP64_LEA_P: vp_lea_p(vp_c1(), vp_dr()); break;
			case VP64_RET: vp_ret(); break;
			case VP64_SYNC: vp_sync(ir >> 8); break;
			case VP64_BRK: vp_brk((ir >> 8) & 0xff); break;
			VP_M_ALL
			case VP64_CMP_F: vp_cmp_f(vp_sr(), vp_dr()); break;
			VP_F_ALL
			case VP64_CPY_IF: vp_cpy_if(vp_sr(), vp_i(), vp_dr()); break;
			case VP64_CPY_FI: vp_cpy_fi(vp_sr(), vp_dr(), vp_i()); break;
			case VP64_CPY_DF: vp_cpy_df(vp_sr(), vp_sc(), vp_dr()); break;
			case VP64_CPY_FD: vp_cpy_fd(vp_dr(), vp_sr(), vp_sc()); break;
			default: std::cout << "Unrecognised opcode " << (ir & 0xff) << " " << std::endl; break;
		}
	}
	return 0;
}