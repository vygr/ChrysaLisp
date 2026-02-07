#include <sys/types.h>
#include <iostream>
#include <atomic>
#include <cmath>
#ifdef _WIN64
	#include <immintrin.h>
#endif
#include "pii.h"

enum Opcodes
{
	VP64_CPY_CR_0,
	VP64_CPY_CR_1,
	VP64_CPY_CR_2,
	VP64_CPY_CR_3,
	VP64_ADD_CR_0,
	VP64_ADD_CR_1,
	VP64_ADD_CR_2,
	VP64_ADD_CR_3,
	VP64_SUB_CR_0,
	VP64_SUB_CR_1,
	VP64_SUB_CR_2,
	VP64_SUB_CR_3,
	VP64_CMP_CR_0,
	VP64_CMP_CR_1,
	VP64_CMP_CR_2,
	VP64_CMP_CR_3,
	VP64_MUL_CR_0,
	VP64_MUL_CR_1,
	VP64_MUL_CR_2,
	VP64_MUL_CR_3,
	VP64_AND_CR_0,
	VP64_AND_CR_1,
	VP64_AND_CR_2,
	VP64_AND_CR_3,
	VP64_OR_CR_0,
	VP64_OR_CR_1,
	VP64_OR_CR_2,
	VP64_OR_CR_3,
	VP64_XOR_CR_0,
	VP64_XOR_CR_1,
	VP64_XOR_CR_2,
	VP64_XOR_CR_3,

	VP64_SHL_CR,
	VP64_SHR_CR,
	VP64_ASR_CR,
	
	VP64_CPY_RR,
	VP64_ADD_RR,
	VP64_SUB_RR,
	VP64_CMP_RR,
	VP64_MUL_RR,
	VP64_AND_RR,
	VP64_OR_RR,
	VP64_XOR_RR,
	VP64_SHL_RR,
	VP64_SHR_RR,
	VP64_ASR_RR,
	VP64_LNOT_RR,
	VP64_LAND_RR,
	VP64_SWP_RR,
	VP64_EXT_RR,
	VP64_DIV_RRR,
	VP64_DIV_RRR_U,

	VP64_SEQ_CR_0,
	VP64_SEQ_CR_1,
	VP64_SEQ_CR_2,
	
	VP64_SNE_CR_0,
	VP64_SNE_CR_1,
	VP64_SNE_CR_2,
	
	VP64_SLT_CR_0,
	VP64_SLT_CR_1,
	VP64_SLT_CR_2,
	
	VP64_SLE_CR_0,
	VP64_SLE_CR_1,
	VP64_SLE_CR_2,
	
	VP64_SGT_CR_0,
	VP64_SGT_CR_1,
	VP64_SGT_CR_2,
	
	VP64_SGE_CR_0,
	VP64_SGE_CR_1,
	VP64_SGE_CR_2,
	
	VP64_SEQ_RR,
	VP64_SNE_RR,
	VP64_SLT_RR,
	VP64_SLE_RR,
	VP64_SGT_RR,
	VP64_SGE_RR,

	VP64_BEQ_0,
	VP64_BEQ_1,
	VP64_BNE_0,
	VP64_BNE_1,
	VP64_BGE_0,
	VP64_BGE_1,
	VP64_BLT_0,
	VP64_BLT_1,
	VP64_BLE_0,
	VP64_BLE_1,
	VP64_BGT_0,
	VP64_BGT_1,

	VP64_CPY_IR_0,
	VP64_CPY_IR_B_0,
	VP64_CPY_IR_S_0,
	VP64_CPY_IR_I_0,
	VP64_CPY_IR_UB_0,
	VP64_CPY_IR_US_0,
	VP64_CPY_IR_UI_0,
	VP64_LEA_I_0,

	VP64_CPY_RI_0,
	VP64_CPY_RI_B_0,
	VP64_CPY_RI_S_0,
	VP64_CPY_RI_I_0,

	VP64_CPY_RD,
	VP64_CPY_RD_B,
	VP64_CPY_RD_S,
	VP64_CPY_RD_I,

	VP64_CPY_DR,
	VP64_CPY_DR_B,
	VP64_CPY_DR_S,
	VP64_CPY_DR_I,
	VP64_CPY_DR_UB,
	VP64_CPY_DR_US,
	VP64_CPY_DR_UI,
	VP64_LEA_D,

	VP64_CALL_R,
	VP64_JMP_R,

	VP64_CALL_I,
	VP64_JMP_I,

	VP64_CPY_PR,
	VP64_LEA_P,

	VP64_CALL_0,
	VP64_CALL_1,
	VP64_JMP_0,
	VP64_JMP_1,

	VP64_CALL_P_0,
	VP64_CALL_P_1,
	VP64_JMP_P_0,
	VP64_JMP_P_1,

	VP64_CALL_ABI,

	VP64_RET,
	VP64_SYNC,
	VP64_BRK,

	VP64_MIN_CR_0,
	VP64_MIN_CR_1,
	VP64_MIN_CR_2,
	VP64_MIN_CR_3,

	VP64_MAX_CR_0,
	VP64_MAX_CR_1,
	VP64_MAX_CR_2,
	VP64_MAX_CR_3,

	VP64_MIN_RR,
	VP64_MAX_RR,
	VP64_ABS_RR,

	VP64_CMP_F,

	VP64_CPY_FF,
	VP64_CPY_RF,
	VP64_CPY_FR,

	VP64_CVT_RF,
	VP64_CVT_FR,

	VP64_ADD_FF,
	VP64_SUB_FF,
	VP64_MUL_FF,
	VP64_DIV_FF,
	VP64_MIN_FF,
	VP64_MAX_FF,
	VP64_SQRT_FF,

	VP64_FBEQ_0,
	VP64_FBEQ_1,
	VP64_FBNE_0,
	VP64_FBNE_1,
	VP64_FBGE_0,
	VP64_FBGE_1,
	VP64_FBLT_0,
	VP64_FBLT_1,
	VP64_FBLE_0,
	VP64_FBLE_1,
	VP64_FBGT_0,
	VP64_FBGT_1,

	VP64_CPY_IF,
	VP64_CPY_FI,

	VP64_ABS_FF,
	VP64_NEG_FF,

	VP64_CPY_DF,
	VP64_CPY_FD,
};

// VP macros
#include <atomic>
#ifdef _WIN64
#include <intrin.h>
#endif

struct i128 { int64_t lo; int64_t hi; };
struct u128 { uint64_t lo; uint64_t hi; };

// Arithmetic & Logic (CR)
#define vp_cpy_cr(c, dr) regs[dr] = (c)
#define vp_add_cr(c, dr) regs[dr] += (c)
#define vp_sub_cr(c, dr) regs[dr] -= (c)
#define vp_cmp_cr(c, dr) compare1 = regs[dr]; compare2 = (c)
#define vp_mul_cr(c, dr) regs[dr] *= (c)
#define vp_and_cr(c, dr) regs[dr] &= (c)
#define vp_or_cr(c, dr) regs[dr] |= (c)
#define vp_xor_cr(c, dr) regs[dr] ^= (c)
#define vp_shl_cr(val, dr) regs[dr] <<= (val)
#define vp_shr_cr(val, dr) regs[dr] = (uint64_t)regs[dr] >> (val)
#define vp_asr_cr(val, dr) regs[dr] >>= (val)

#define vp_seq_cr(c, dr) regs[dr] = regs[dr] == (c)
#define vp_sne_cr(c, dr) regs[dr] = regs[dr] != (c)
#define vp_slt_cr(c, dr) regs[dr] = regs[dr] < (c)
#define vp_sle_cr(c, dr) regs[dr] = regs[dr] <= (c)
#define vp_sgt_cr(c, dr) regs[dr] = regs[dr] > (c)
#define vp_sge_cr(c, dr) regs[dr] = regs[dr] >= (c)

#define vp_min_cr(c, dr) { int64_t val = (c); if (val < regs[dr]) regs[dr] = val; }
#define vp_max_cr(c, dr) { int64_t val = (c); if (val > regs[dr]) regs[dr] = val; }

// Arithmetic & Logic (RR)
#define vp_cpy_rr(sr, dr) regs[dr] = regs[sr]
#define vp_add_rr(sr, dr) regs[dr] += regs[sr]
#define vp_sub_rr(sr, dr) regs[dr] -= regs[sr]
#define vp_cmp_rr(sr, dr) compare1 = regs[dr]; compare2 = regs[sr]
#define vp_mul_rr(sr, dr) regs[dr] *= regs[sr]
#define vp_and_rr(sr, dr) regs[dr] &= regs[sr]
#define vp_or_rr(sr, dr) regs[dr] |= regs[sr]
#define vp_xor_rr(sr, dr) regs[dr] ^= regs[sr]
#define vp_shl_rr(sr, dr) regs[dr] <<= regs[sr]
#define vp_shr_rr(sr, dr) regs[dr] = (uint64_t)regs[dr] >> regs[sr]
#define vp_asr_rr(sr, dr) regs[dr] >>= regs[sr]
#define vp_lnot_rr(sr, dr) regs[dr] = !regs[dr]
#define vp_land_rr(sr, dr) regs[dr] = regs[dr] && regs[sr]
#define vp_swp_rr(sr, dr) { int64_t t = regs[dr]; regs[dr] = regs[sr]; regs[sr] = t; }
#define vp_ext_rr(sr, dr) regs[dr] = (regs[sr] >> 63)

#ifdef _WIN64
#define vp_div_rrr(sr, dr, divisor_reg) regs[dr] = _div128(regs[sr], regs[dr], regs[divisor_reg], &regs[sr])
#define vp_div_rrr_u(sr, dr, divisor_reg) regs[dr] = _udiv128((uint64_t)regs[sr], (uint64_t)regs[dr], (uint64_t)regs[divisor_reg], (uint64_t*)&regs[sr])
#else
#define vp_div_rrr(sr, dr, divisor_reg) { \
	i128 value = {regs[dr], regs[sr]}; \
	int64_t div = regs[divisor_reg]; \
	regs[dr] = (__int128_t&)value / div; \
	regs[sr] = (__int128_t&)value % div; \
}
#define vp_div_rrr_u(sr, dr, divisor_reg) { \
	u128 value = {(uint64_t)regs[dr], (uint64_t)regs[sr]}; \
	uint64_t div = (uint64_t)regs[divisor_reg]; \
	regs[dr] = (__uint128_t&)value / div; \
	regs[sr] = (__uint128_t&)value % div; \
}
#endif

#define vp_seq_rr(sr, dr) regs[dr] = regs[dr] == regs[sr]
#define vp_sne_rr(sr, dr) regs[dr] = regs[dr] != regs[sr]
#define vp_slt_rr(sr, dr) regs[dr] = regs[dr] < regs[sr]
#define vp_sle_rr(sr, dr) regs[dr] = regs[dr] <= regs[sr]
#define vp_sgt_rr(sr, dr) regs[dr] = regs[dr] > regs[sr]
#define vp_sge_rr(sr, dr) regs[dr] = regs[dr] >= regs[sr]

#define vp_min_rr(sr, dr) if (regs[sr] < regs[dr]) regs[dr] = regs[sr]
#define vp_max_rr(sr, dr) if (regs[sr] > regs[dr]) regs[dr] = regs[sr]
#define vp_abs_rr(sr, dr) { int64_t val = regs[sr]; regs[dr] = (val < 0) ? -val : val; }

// Memory (IR/RI)
#define vp_cpy_ir(sr, offset, dr) regs[dr] = (int64_t)*(int64_t*)(regs[sr] + offset)
#define vp_cpy_ir_b(sr, offset, dr) regs[dr] = (int64_t)*(int8_t*)(regs[sr] + offset)
#define vp_cpy_ir_s(sr, offset, dr) regs[dr] = (int64_t)*(int16_t*)(regs[sr] + offset)
#define vp_cpy_ir_i(sr, offset, dr) regs[dr] = (int64_t)*(int32_t*)(regs[sr] + offset)
#define vp_cpy_ir_ub(sr, offset, dr) regs[dr] = (uint64_t)*(uint8_t*)(regs[sr] + offset)
#define vp_cpy_ir_us(sr, offset, dr) regs[dr] = (uint64_t)*(uint16_t*)(regs[sr] + offset)
#define vp_cpy_ir_ui(sr, offset, dr) regs[dr] = (uint64_t)*(uint32_t*)(regs[sr] + offset)
#define vp_lea_i(sr, offset, dr) regs[dr] = regs[sr] + offset

#define vp_cpy_ri(sr, dr, offset) *(int64_t*)(regs[dr] + offset) = regs[sr]
#define vp_cpy_ri_b(sr, dr, offset) *(int8_t*)(regs[dr] + offset) = (int8_t)regs[sr]
#define vp_cpy_ri_s(sr, dr, offset) *(int16_t*)(regs[dr] + offset) = (int16_t)regs[sr]
#define vp_cpy_ri_i(sr, dr, offset) *(int32_t*)(regs[dr] + offset) = (int32_t)regs[sr]

// Memory (DR/RD) - Indirect addressing (Reg + Reg)
#define vp_cpy_rd(sr_idx, base_idx, dr_idx) *(uint64_t*)(regs[base_idx] + regs[dr_idx]) = regs[sr_idx]
#define vp_cpy_rd_b(sr_idx, base_idx, dr_idx) *(uint8_t*)(regs[base_idx] + regs[dr_idx]) = (uint8_t)regs[sr_idx]
#define vp_cpy_rd_s(sr_idx, base_idx, dr_idx) *(uint16_t*)(regs[base_idx] + regs[dr_idx]) = (uint16_t)regs[sr_idx]
#define vp_cpy_rd_i(sr_idx, base_idx, dr_idx) *(uint32_t*)(regs[base_idx] + regs[dr_idx]) = (uint32_t)regs[sr_idx]

#define vp_cpy_dr(base_idx, sr_idx, dr_idx) regs[dr_idx] = (uint64_t)*(int64_t*)(regs[base_idx] + regs[sr_idx])
#define vp_cpy_dr_b(base_idx, sr_idx, dr_idx) regs[dr_idx] = (uint64_t)*(int8_t*)(regs[base_idx] + regs[sr_idx])
#define vp_cpy_dr_s(base_idx, sr_idx, dr_idx) regs[dr_idx] = (uint64_t)*(int16_t*)(regs[base_idx] + regs[sr_idx])
#define vp_cpy_dr_i(base_idx, sr_idx, dr_idx) regs[dr_idx] = (uint64_t)*(int32_t*)(regs[base_idx] + regs[sr_idx])
#define vp_cpy_dr_ub(base_idx, sr_idx, dr_idx) regs[dr_idx] = (uint64_t)*(uint8_t*)(regs[base_idx] + regs[sr_idx])
#define vp_cpy_dr_us(base_idx, sr_idx, dr_idx) regs[dr_idx] = (uint64_t)*(uint16_t*)(regs[base_idx] + regs[sr_idx])
#define vp_cpy_dr_ui(base_idx, sr_idx, dr_idx) regs[dr_idx] = (uint64_t)*(uint32_t*)(regs[base_idx] + regs[sr_idx])
#define vp_lea_d(base_idx, sr_idx, dr_idx) regs[dr_idx] = regs[base_idx] + regs[sr_idx]

// Control Flow
#define vp_beq(offset) { int64_t _o = (offset); if (compare1 == compare2) pc = (int16_t*)((char*)pc + _o); }
#define vp_bne(offset) { int64_t _o = (offset); if (compare1 != compare2) pc = (int16_t*)((char*)pc + _o); }
#define vp_bge(offset) { int64_t _o = (offset); if (compare1 >= compare2) pc = (int16_t*)((char*)pc + _o); }
#define vp_blt(offset) { int64_t _o = (offset); if (compare1 < compare2) pc = (int16_t*)((char*)pc + _o); }
#define vp_ble(offset) { int64_t _o = (offset); if (compare1 <= compare2) pc = (int16_t*)((char*)pc + _o); }
#define vp_bgt(offset) { int64_t _o = (offset); if (compare1 > compare2) pc = (int16_t*)((char*)pc + _o); }

#define vp_call(offset) { int64_t _o = (offset); regs[15] -= 8; *(int16_t**)regs[15] = pc; pc = (int16_t*)((char*)pc + _o); }
#define vp_jmp(offset) { int64_t _o = (offset); pc = (int16_t*)((char*)pc + _o); }

#define vp_call_p(offset) { int64_t _o = (offset); regs[15] -= 8; *(int16_t**)regs[15] = pc; pc = *(int16_t**)((char*)pc + _o); }
#define vp_jmp_p(offset) { int64_t _o = (offset); pc = *(int16_t**)((char*)pc + _o); }

#define vp_call_r(dr) { regs[15] -= 8; *(int16_t**)regs[15] = pc; pc = (int16_t*)regs[dr]; }
#define vp_jmp_r(dr) pc = (int16_t*)regs[dr]

#define vp_call_i(base_reg, offset) { \
	int64_t base = regs[base_reg]; \
	int64_t _o = (offset); \
	regs[15] -= 8; \
	*(int16_t**)regs[15] = pc; \
	pc = *(int16_t**)(base + _o); \
}
#define vp_jmp_i(base_reg, offset) { \
	int64_t base = regs[base_reg]; \
	int64_t _o = (offset); \
	pc = *(int16_t**)(base + _o); \
}

#define vp_call_abi(n, base, offset) { \
	int64_t fptr_addr = *(uint64_t*)(regs[base] + (offset)); \
	switch (n) \
	{ \
		case 0: regs[0] = (((uint64_t(*)(void))fptr_addr)()); break; \
		case 1: regs[0] = (((uint64_t(*)(uint64_t))fptr_addr)(regs[0])); break; \
		case 2: regs[0] = (((uint64_t(*)(uint64_t, uint64_t))fptr_addr)(regs[0], regs[1])); break; \
		case 3: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t))fptr_addr)(regs[0], regs[1], regs[2])); break; \
		case 4: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t))fptr_addr)(regs[0], regs[1], regs[2], regs[3])); break; \
		case 5: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4])); break; \
		case 6: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5])); break; \
		case 7: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6])); break; \
		case 8: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7])); break; \
		case 9: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8])); break; \
		case 10: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t))fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9])); break; \
		case 11: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10])); break; \
		case 12: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10], regs[11])); break; \
		case 13: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10], regs[11], regs[12])); break; \
		case 14: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10], regs[11], regs[12], regs[13])); break; \
		case 15: regs[0] = (((uint64_t(*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t)) fptr_addr)(regs[0], regs[1], regs[2], regs[3], regs[4], regs[5], regs[6], regs[7], regs[8], regs[9], regs[10], regs[11], regs[12], regs[13], regs[14])); break; \
	} \
}

#define vp_cpy_pr(offset, dr) { int64_t _o = (offset); regs[dr] = *(int64_t*)((char*)pc + _o); }
#define vp_lea_p(offset, dr) { int64_t _o = (offset); regs[dr] = (int64_t)((char*)pc + _o); }

#define vp_ret() { pc = *(int16_t**)regs[15]; regs[15] += 8; }
#define vp_sync(code) sync.store((code), std::memory_order_seq_cst)
#define vp_brk(code) std::cout << "brk " << (int)(code) << std::endl

// Floating Point
#define vp_add_ff(sr, dr) fregs[dr] += fregs[sr]
#define vp_sub_ff(sr, dr) fregs[dr] -= fregs[sr]
#define vp_mul_ff(sr, dr) fregs[dr] *= fregs[sr]
#define vp_div_ff(sr, dr) fregs[dr] /= fregs[sr]
#define vp_min_ff(sr, dr) fregs[dr] = std::fmin(fregs[dr], fregs[sr])
#define vp_max_ff(sr, dr) fregs[dr] = std::fmax(fregs[dr], fregs[sr])
#define vp_sqrt_ff(sr, dr) fregs[dr] = std::sqrt(fregs[sr])
#define vp_abs_ff(sr, dr) fregs[dr] = std::abs(fregs[sr])
#define vp_neg_ff(sr, dr) fregs[dr] = -fregs[sr]

#define vp_fbeq(offset) { int64_t _o = (offset); if (compare_f1 == compare_f2) pc = (int16_t*)((char*)pc + _o); }
#define vp_fbne(offset) { int64_t _o = (offset); if (compare_f1 != compare_f2) pc = (int16_t*)((char*)pc + _o); }
#define vp_fbge(offset) { int64_t _o = (offset); if (compare_f2 >= compare_f1) pc = (int16_t*)((char*)pc + _o); }
#define vp_fblt(offset) { int64_t _o = (offset); if (compare_f2 < compare_f1) pc = (int16_t*)((char*)pc + _o); }
#define vp_fble(offset) { int64_t _o = (offset); if (compare_f2 <= compare_f1) pc = (int16_t*)((char*)pc + _o); }
#define vp_fbgt(offset) { int64_t _o = (offset); if (compare_f2 > compare_f1) pc = (int16_t*)((char*)pc + _o); }

#define vp_cpy_if(base_reg, offset, dr) fregs[dr] = *(double*)(regs[base_reg] + offset)
#define vp_cpy_fi(sr, base_reg, offset) *(double*)(regs[base_reg] + offset) = fregs[sr]
#define vp_cpy_df(base_reg, index_reg, dr) fregs[dr] = *(double*)(regs[base_reg] + regs[index_reg])
#define vp_cpy_fd(sr, base_reg, index_reg) *(double*)(regs[base_reg] + regs[index_reg]) = fregs[sr]

#define vp_cvt_rf(sr, dr) fregs[dr] = (double)regs[sr]
#define vp_cvt_fr(sr, dr) regs[dr] = (int64_t)fregs[sr]

#define vp_cpy_ff(sr, dr) fregs[dr] = fregs[sr]
#define vp_cpy_rf(sr, dr) *(int64_t*)&fregs[dr] = regs[sr]
#define vp_cpy_fr(sr, dr) regs[dr] = *(int64_t*)&fregs[sr]
#define vp_cmp_f(sr, dr) compare_f1 = fregs[sr]; compare_f2 = fregs[dr]

// System Helpers
#define vp_reg(n) regs[n]
#define vp_abi_ret(val) regs[0] = val
#define vp_abi_fetch_ptr(base_reg, offset) *(uint64_t*)(regs[base_reg] + (offset))
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

	// start at the beginning
	fn_header* pHeader = (fn_header*)((uint8_t*)data);
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
			case VP64_CPY_CR_0: vp_cpy_cr(vp_c0(), vp_dr()); break;
			case VP64_CPY_CR_1: vp_cpy_cr(vp_c1(), vp_dr()); break;
			case VP64_CPY_CR_2: vp_cpy_cr(vp_c2(), vp_dr()); break;
			case VP64_CPY_CR_3: vp_cpy_cr(vp_c3(), vp_dr()); break;

			case VP64_ADD_CR_0: vp_add_cr(vp_c0(), vp_dr()); break;
			case VP64_ADD_CR_1: vp_add_cr(vp_c1(), vp_dr()); break;
			case VP64_ADD_CR_2: vp_add_cr(vp_c2(), vp_dr()); break;
			case VP64_ADD_CR_3: vp_add_cr(vp_c3(), vp_dr()); break;

			case VP64_SUB_CR_0: vp_sub_cr(vp_c0(), vp_dr()); break;
			case VP64_SUB_CR_1: vp_sub_cr(vp_c1(), vp_dr()); break;
			case VP64_SUB_CR_2: vp_sub_cr(vp_c2(), vp_dr()); break;
			case VP64_SUB_CR_3: vp_sub_cr(vp_c3(), vp_dr()); break;

			case VP64_CMP_CR_0: vp_cmp_cr(vp_c0(), vp_dr()); break;
			case VP64_CMP_CR_1: vp_cmp_cr(vp_c1(), vp_dr()); break;
			case VP64_CMP_CR_2: vp_cmp_cr(vp_c2(), vp_dr()); break;
			case VP64_CMP_CR_3: vp_cmp_cr(vp_c3(), vp_dr()); break;

			case VP64_MUL_CR_0: vp_mul_cr(vp_c0(), vp_dr()); break;
			case VP64_MUL_CR_1: vp_mul_cr(vp_c1(), vp_dr()); break;
			case VP64_MUL_CR_2: vp_mul_cr(vp_c2(), vp_dr()); break;
			case VP64_MUL_CR_3: vp_mul_cr(vp_c3(), vp_dr()); break;

			case VP64_AND_CR_0: vp_and_cr(vp_c0(), vp_dr()); break;
			case VP64_AND_CR_1: vp_and_cr(vp_c1(), vp_dr()); break;
			case VP64_AND_CR_2: vp_and_cr(vp_c2(), vp_dr()); break;
			case VP64_AND_CR_3: vp_and_cr(vp_c3(), vp_dr()); break;

			case VP64_OR_CR_0: vp_or_cr(vp_c0(), vp_dr()); break;
			case VP64_OR_CR_1: vp_or_cr(vp_c1(), vp_dr()); break;
			case VP64_OR_CR_2: vp_or_cr(vp_c2(), vp_dr()); break;
			case VP64_OR_CR_3: vp_or_cr(vp_c3(), vp_dr()); break;

			case VP64_XOR_CR_0: vp_xor_cr(vp_c0(), vp_dr()); break;
			case VP64_XOR_CR_1: vp_xor_cr(vp_c1(), vp_dr()); break;
			case VP64_XOR_CR_2: vp_xor_cr(vp_c2(), vp_dr()); break;
			case VP64_XOR_CR_3: vp_xor_cr(vp_c3(), vp_dr()); break;

			case VP64_SHL_CR: vp_shl_cr(vp_sc(), vp_dr()); break;
			case VP64_SHR_CR: vp_shr_cr(vp_sc(), vp_dr()); break;
			case VP64_ASR_CR: vp_asr_cr(vp_sc(), vp_dr()); break;

			case VP64_CPY_RR: vp_cpy_rr(vp_sr(), vp_dr()); break;
			case VP64_ADD_RR: vp_add_rr(vp_sr(), vp_dr()); break;
			case VP64_SUB_RR: vp_sub_rr(vp_sr(), vp_dr()); break;
			case VP64_CMP_RR: vp_cmp_rr(vp_sr(), vp_dr()); break;
			case VP64_MUL_RR: vp_mul_rr(vp_sr(), vp_dr()); break;
			case VP64_AND_RR: vp_and_rr(vp_sr(), vp_dr()); break;
			case VP64_OR_RR: vp_or_rr(vp_sr(), vp_dr()); break;
			case VP64_XOR_RR: vp_xor_rr(vp_sr(), vp_dr()); break;
			case VP64_SHL_RR: vp_shl_rr(vp_sr(), vp_dr()); break;
			case VP64_SHR_RR: vp_shr_rr(vp_sr(), vp_dr()); break;
			case VP64_ASR_RR: vp_asr_rr(vp_sr(), vp_dr()); break;
			case VP64_LNOT_RR: vp_lnot_rr(vp_sr(), vp_dr()); break;
			case VP64_LAND_RR: vp_land_rr(vp_sr(), vp_dr()); break;
			case VP64_SWP_RR: vp_swp_rr(vp_sr(), vp_dr()); break;
			case VP64_EXT_RR: vp_ext_rr(vp_sr(), vp_dr()); break;

			case VP64_DIV_RRR: vp_div_rrr(vp_sr(), vp_dr(), vp_sc()); break;
			case VP64_DIV_RRR_U: vp_div_rrr_u(vp_sr(), vp_dr(), vp_sc()); break;

			case VP64_SEQ_CR_0: vp_seq_cr(vp_c0(), vp_dr()); break;
			case VP64_SEQ_CR_1: vp_seq_cr(vp_c1(), vp_dr()); break;
			case VP64_SEQ_CR_2: vp_seq_cr(vp_c2(), vp_dr()); break;

			case VP64_SNE_CR_0: vp_sne_cr(vp_c0(), vp_dr()); break;
			case VP64_SNE_CR_1: vp_sne_cr(vp_c1(), vp_dr()); break;
			case VP64_SNE_CR_2: vp_sne_cr(vp_c2(), vp_dr()); break;

			case VP64_SLT_CR_0: vp_slt_cr(vp_c0(), vp_dr()); break;
			case VP64_SLT_CR_1: vp_slt_cr(vp_c1(), vp_dr()); break;
			case VP64_SLT_CR_2: vp_slt_cr(vp_c2(), vp_dr()); break;

			case VP64_SLE_CR_0: vp_sle_cr(vp_c0(), vp_dr()); break;
			case VP64_SLE_CR_1: vp_sle_cr(vp_c1(), vp_dr()); break;
			case VP64_SLE_CR_2: vp_sle_cr(vp_c2(), vp_dr()); break;

			case VP64_SGT_CR_0: vp_sgt_cr(vp_c0(), vp_dr()); break;
			case VP64_SGT_CR_1: vp_sgt_cr(vp_c1(), vp_dr()); break;
			case VP64_SGT_CR_2: vp_sgt_cr(vp_c2(), vp_dr()); break;

			case VP64_SGE_CR_0: vp_sge_cr(vp_c0(), vp_dr()); break;
			case VP64_SGE_CR_1: vp_sge_cr(vp_c1(), vp_dr()); break;
			case VP64_SGE_CR_2: vp_sge_cr(vp_c2(), vp_dr()); break;

			case VP64_SEQ_RR: vp_seq_rr(vp_sr(), vp_dr()); break;
			case VP64_SNE_RR: vp_sne_rr(vp_sr(), vp_dr()); break;
			case VP64_SLT_RR: vp_slt_rr(vp_sr(), vp_dr()); break;
			case VP64_SLE_RR: vp_sle_rr(vp_sr(), vp_dr()); break;
			case VP64_SGT_RR: vp_sgt_rr(vp_sr(), vp_dr()); break;
			case VP64_SGE_RR: vp_sge_rr(vp_sr(), vp_dr()); break;

			case VP64_BEQ_0: vp_beq(vp_o0()); break;
			case VP64_BEQ_1: vp_beq(vp_o1()); break;
			case VP64_BNE_0: vp_bne(vp_o0()); break;
			case VP64_BNE_1: vp_bne(vp_o1()); break;
			case VP64_BGE_0: vp_bge(vp_o0()); break;
			case VP64_BGE_1: vp_bge(vp_o1()); break;
			case VP64_BLT_0: vp_blt(vp_o0()); break;
			case VP64_BLT_1: vp_blt(vp_o1()); break;
			case VP64_BLE_0: vp_ble(vp_o0()); break;
			case VP64_BLE_1: vp_ble(vp_o1()); break;
			case VP64_BGT_0: vp_bgt(vp_o0()); break;
			case VP64_BGT_1: vp_bgt(vp_o1()); break;

			case VP64_CPY_IR_0: vp_cpy_ir(vp_sr(), vp_i(), vp_dr()); break;
			case VP64_CPY_IR_B_0: vp_cpy_ir_b(vp_sr(), vp_i(), vp_dr()); break;
			case VP64_CPY_IR_S_0: vp_cpy_ir_s(vp_sr(), vp_i(), vp_dr()); break;
			case VP64_CPY_IR_I_0: vp_cpy_ir_i(vp_sr(), vp_i(), vp_dr()); break;
			case VP64_CPY_IR_UB_0: vp_cpy_ir_ub(vp_sr(), vp_i(), vp_dr()); break;
			case VP64_CPY_IR_US_0: vp_cpy_ir_us(vp_sr(), vp_i(), vp_dr()); break;
			case VP64_CPY_IR_UI_0: vp_cpy_ir_ui(vp_sr(), vp_i(), vp_dr()); break;
			case VP64_LEA_I_0: vp_lea_i(vp_sr(), vp_i(), vp_dr()); break;

			case VP64_CPY_RI_0: vp_cpy_ri(vp_sr(), vp_dr(), vp_i()); break;
			case VP64_CPY_RI_B_0: vp_cpy_ri_b(vp_sr(), vp_dr(), vp_i()); break;
			case VP64_CPY_RI_S_0: vp_cpy_ri_s(vp_sr(), vp_dr(), vp_i()); break;
			case VP64_CPY_RI_I_0: vp_cpy_ri_i(vp_sr(), vp_dr(), vp_i()); break;

			case VP64_CPY_RD: vp_cpy_rd(vp_sc(), vp_dr(), vp_sr()); break;
			case VP64_CPY_RD_B: vp_cpy_rd_b(vp_sc(), vp_dr(), vp_sr()); break;
			case VP64_CPY_RD_S: vp_cpy_rd_s(vp_sc(), vp_dr(), vp_sr()); break;
			case VP64_CPY_RD_I: vp_cpy_rd_i(vp_sc(), vp_dr(), vp_sr()); break;

			case VP64_CPY_DR: vp_cpy_dr(vp_sr(), vp_sc(), vp_dr()); break;
			case VP64_CPY_DR_B: vp_cpy_dr_b(vp_sr(), vp_sc(), vp_dr()); break;
			case VP64_CPY_DR_S: vp_cpy_dr_s(vp_sr(), vp_sc(), vp_dr()); break;
			case VP64_CPY_DR_I: vp_cpy_dr_i(vp_sr(), vp_sc(), vp_dr()); break;
			case VP64_CPY_DR_UB: vp_cpy_dr_ub(vp_sr(), vp_sc(), vp_dr()); break;
			case VP64_CPY_DR_US: vp_cpy_dr_us(vp_sr(), vp_sc(), vp_dr()); break;
			case VP64_CPY_DR_UI: vp_cpy_dr_ui(vp_sr(), vp_sc(), vp_dr()); break;
			case VP64_LEA_D: vp_lea_d(vp_sr(), vp_sc(), vp_dr()); break;

			case VP64_CALL_0: vp_call(vp_o0()); break;
			case VP64_CALL_1: vp_call(vp_o1()); break;
			case VP64_JMP_0: vp_jmp(vp_o0()); break;
			case VP64_JMP_1: vp_jmp(vp_o1()); break;
			case VP64_CALL_P_0: vp_call_p(vp_o0()); break;
			case VP64_CALL_P_1: vp_call_p(vp_o1()); break;
			case VP64_JMP_P_0: vp_jmp_p(vp_o0()); break;
			case VP64_JMP_P_1: vp_jmp_p(vp_o1()); break;

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

			case VP64_MIN_CR_0: vp_min_cr(vp_c0(), vp_dr()); break;
			case VP64_MIN_CR_1: vp_min_cr(vp_c1(), vp_dr()); break;
			case VP64_MIN_CR_2: vp_min_cr(vp_c2(), vp_dr()); break;
			case VP64_MIN_CR_3: vp_min_cr(vp_c3(), vp_dr()); break;
			case VP64_MAX_CR_0: vp_max_cr(vp_c0(), vp_dr()); break;
			case VP64_MAX_CR_1: vp_max_cr(vp_c1(), vp_dr()); break;
			case VP64_MAX_CR_2: vp_max_cr(vp_c2(), vp_dr()); break;
			case VP64_MAX_CR_3: vp_max_cr(vp_c3(), vp_dr()); break;

			case VP64_MIN_RR: vp_min_rr(vp_sr(), vp_dr()); break;
			case VP64_MAX_RR: vp_max_rr(vp_sr(), vp_dr()); break;
			case VP64_ABS_RR: vp_abs_rr(vp_sr(), vp_dr()); break;

			case VP64_CMP_F: vp_cmp_f(vp_sr(), vp_dr()); break;
			case VP64_CPY_FF: vp_cpy_ff(vp_sr(), vp_dr()); break;
			case VP64_CPY_RF: vp_cpy_rf(vp_sr(), vp_dr()); break;
			case VP64_CPY_FR: vp_cpy_fr(vp_sr(), vp_dr()); break;
			case VP64_CVT_RF: vp_cvt_rf(vp_sr(), vp_dr()); break;
			case VP64_CVT_FR: vp_cvt_fr(vp_sr(), vp_dr()); break;
			case VP64_ADD_FF: vp_add_ff(vp_sr(), vp_dr()); break;
			case VP64_SUB_FF: vp_sub_ff(vp_sr(), vp_dr()); break;
			case VP64_MUL_FF: vp_mul_ff(vp_sr(), vp_dr()); break;
			case VP64_DIV_FF: vp_div_ff(vp_sr(), vp_dr()); break;
			case VP64_MIN_FF: vp_min_ff(vp_sr(), vp_dr()); break;
			case VP64_MAX_FF: vp_max_ff(vp_sr(), vp_dr()); break;
			case VP64_SQRT_FF: vp_sqrt_ff(vp_sr(), vp_dr()); break;

			case VP64_FBEQ_0: vp_fbeq(vp_o0()); break;
			case VP64_FBEQ_1: vp_fbeq(vp_offset()); break;
			case VP64_FBNE_0: vp_fbne(vp_o0()); break;
			case VP64_FBNE_1: vp_fbne(vp_offset()); break;
			case VP64_FBLT_0: vp_fblt(vp_o0()); break;
			case VP64_FBLT_1: vp_fblt(vp_offset()); break;
			case VP64_FBLE_0: vp_fble(vp_o0()); break;
			case VP64_FBLE_1: vp_fble(vp_offset()); break;
			case VP64_FBGT_0: vp_fbgt(vp_o0()); break;
			case VP64_FBGT_1: vp_fbgt(vp_offset()); break;
			case VP64_FBGE_0: vp_fbge(vp_o0()); break;
			case VP64_FBGE_1: vp_fbge(vp_offset()); break;

			case VP64_CPY_IF: vp_cpy_if(vp_sr(), vp_i(), vp_dr()); break;
			case VP64_CPY_FI: vp_cpy_fi(vp_sr(), vp_dr(), vp_i()); break;
			case VP64_CPY_DF: vp_cpy_df(vp_sr(), vp_sc(), vp_dr()); break;
			case VP64_CPY_FD: vp_cpy_fd(vp_dr(), vp_sr(), vp_sc()); break;
			case VP64_ABS_FF: vp_abs_ff(vp_sr(), vp_dr()); break;
			case VP64_NEG_FF: vp_neg_ff(vp_sr(), vp_dr()); break;

			default:
			{
				std::cout << "Unrecognised opcode " << (ir & 0xff) << " " << std::endl;
				break;
			}
			break;
		}
	}
	return 0;
}