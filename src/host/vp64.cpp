#include <sys/types.h>
#include <iostream>
#include <atomic>
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
};

std::string opcodeDesc[] = {
	"VP64_CPY_CR_0",
	"VP64_CPY_CR_1",
	"VP64_CPY_CR_2",
	"VP64_CPY_CR_3",
	"VP64_ADD_CR_0",
	"VP64_ADD_CR_1",
	"VP64_ADD_CR_2",
	"VP64_ADD_CR_3",
	"VP64_SUB_CR_0",
	"VP64_SUB_CR_1",
	"VP64_SUB_CR_2",
	"VP64_SUB_CR_3",
	"VP64_CMP_CR_0",
	"VP64_CMP_CR_1",
	"VP64_CMP_CR_2",
	"VP64_CMP_CR_3",
	"VP64_MUL_CR_0",
	"VP64_MUL_CR_1",
	"VP64_MUL_CR_2",
	"VP64_MUL_CR_3",
	"VP64_AND_CR_0",
	"VP64_AND_CR_1",
	"VP64_AND_CR_2",
	"VP64_AND_CR_3",
	"VP64_OR_CR_0",
	"VP64_OR_CR_1",
	"VP64_OR_CR_2",
	"VP64_OR_CR_3",
	"VP64_XOR_CR_0",
	"VP64_XOR_CR_1",
	"VP64_XOR_CR_2",
	"VP64_XOR_CR_3",

	"VP64_SHL_CR",
	"VP64_SHR_CR",
	"VP64_ASR_CR",

	"VP64_CPY_RR",
	"VP64_ADD_RR",
	"VP64_SUB_RR",
	"VP64_CMP_RR",
	"VP64_MUL_RR",
	"VP64_AND_RR",
	"VP64_OR_RR",
	"VP64_XOR_RR",
	"VP64_SHL_RR",
	"VP64_SHR_RR",
	"VP64_ASR_RR",
	"VP64_LNOT_RR",
	"VP64_LAND_RR",
	"VP64_SWP_RR",
	"VP64_EXT_RR",
	"VP64_DIV_RRR",
	"VP64_DIV_RRR_U",

	"VP64_SEQ_CR_0",
	"VP64_SEQ_CR_1",
	"VP64_SEQ_CR_2",

	"VP64_SNE_CR_0",
	"VP64_SNE_CR_1",
	"VP64_SNE_CR_2",

	"VP64_SLT_CR_0",
	"VP64_SLT_CR_1",
	"VP64_SLT_CR_2",

	"VP64_SLE_CR_0",
	"VP64_SLE_CR_1",
	"VP64_SLE_CR_2",

	"VP64_SGT_CR_0",
	"VP64_SGT_CR_1",
	"VP64_SGT_CR_2",

	"VP64_SGE_CR_0",
	"VP64_SGE_CR_1",
	"VP64_SGE_CR_2",

	"VP64_SEQ_RR",
	"VP64_SNE_RR",
	"VP64_SLT_RR",
	"VP64_SLE_RR",
	"VP64_SGT_RR",
	"VP64_SGE_RR",

	"VP64_BEQ_0",
	"VP64_BEQ_1",
	"VP64_BNE_0",
	"VP64_BNE_1",
	"VP64_BGE_0",
	"VP64_BGE_1",
	"VP64_BLT_0",
	"VP64_BLT_1",
	"VP64_BLE_0",
	"VP64_BLE_1",
	"VP64_BGT_0",
	"VP64_BGT_1",

	"VP64_CPY_IR_0",
	"VP64_CPY_IR_B_0",
	"VP64_CPY_IR_S_0",
	"VP64_CPY_IR_I_0",
	"VP64_CPY_IR_UB_0",
	"VP64_CPY_IR_US_0",
	"VP64_CPY_IR_UI_0",
	"VP64_LEA_I_0",

	"VP64_CPY_RI_0",
	"VP64_CPY_RI_B_0",
	"VP64_CPY_RI_S_0",
	"VP64_CPY_RI_I_0",

	"VP64_CPY_RD",
	"VP64_CPY_RD_B",
	"VP64_CPY_RD_S",
	"VP64_CPY_RD_I",

	"VP64_CPY_DR",
	"VP64_CPY_DR_B",
	"VP64_CPY_DR_S",
	"VP64_CPY_DR_I",
	"VP64_CPY_DR_UB",
	"VP64_CPY_DR_US",
	"VP64_CPY_DR_UI",
	"VP64_LEA_D",

	"VP64_CALL_R",
	"VP64_JMP_R",

	"VP64_CALL_I",
	"VP64_JMP_I",

	"VP64_CPY_PR",
	"VP64_LEA_P",

	"VP64_CALL_0",
	"VP64_CALL_1",
	"VP64_JMP_0",
	"VP64_JMP_1",

	"VP64_CALL_P_0",
	"VP64_CALL_P_1",
	"VP64_JMP_P_0",
	"VP64_JMP_P_1",

	"VP64_CALL_ABI",

	"VP64_RET",
	"VP64_SYNC",
	"VP64_BRK"
};

int vp64(uint8_t* data, int64_t *stack, int64_t* argv, int64_t* host_os_funcs, int64_t* host_gui_funcs)
{
	int64_t regs[16];
	int16_t* pc;
	int64_t ir;
	int64_t compare1 = 0;
	int64_t compare2 = 0;
	std::atomic<int> sync;

	// start at the beginning
	fn_header* pHeader = (fn_header*)((uint8_t*)data);
	pc = (int16_t*)((uint8_t*)data + pHeader->entry);
	regs[0] = (int64_t)argv;
	regs[1] = (int64_t)host_os_funcs;
	regs[2] = (int64_t)host_gui_funcs;
	regs[15] = (int64_t)stack;

	for(;;)
	{
		ir = *pc++;
		switch (ir & 0xff)
		{
			case VP64_CPY_CR_0:
			{
				regs[(ir >> 8) & 0xf] = ir >> 12;
			}
			break;

			case VP64_CPY_CR_1:
			{
				regs[(ir >> 8) & 0xf] = ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VP64_CPY_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VP64_CPY_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] = (o0 | o1 | o2 | o3);
			}
			break;
		
			case VP64_ADD_CR_0:
			{
				regs[(ir >> 8) & 0xf] += ir >> 12;
			}
			break;

			case VP64_ADD_CR_1:
			{
				regs[(ir >> 8) & 0xf] += ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VP64_ADD_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] += ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VP64_ADD_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] += (o0 | o1 | o2 | o3);
			}
			break;
		
			case VP64_SUB_CR_0:
			{
				regs[(ir >> 8) & 0xf] -= ir >> 12;
			}
			break;

			case VP64_SUB_CR_1:
			{
				regs[(ir >> 8) & 0xf] -= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VP64_SUB_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] -= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VP64_SUB_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] -= (o0 | o1 | o2 | o3);
			}
			break;

			case VP64_CMP_CR_0:
			{
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = ir >> 12;
			}
			break;

			case VP64_CMP_CR_1:
			{
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VP64_CMP_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VP64_CMP_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*(uint16_t*)pc++ << 48;
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = (o0 | o1 | o2 | o3);
			}
			break;

			case VP64_MUL_CR_0:
			{
				regs[(ir >> 8) & 0xf] *= ir >> 12;
			}
			break;

			case VP64_MUL_CR_1:
			{
				regs[(ir >> 8) & 0xf] *= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VP64_MUL_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] *= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VP64_MUL_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] *= (o0 | o1 | o2 | o3);
			}
			break;

			case VP64_AND_CR_0:
			{
				regs[(ir >> 8) & 0xf] &= ir >> 12;
			}
			break;

			case VP64_AND_CR_1:
			{
				regs[(ir >> 8) & 0xf] &= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VP64_AND_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] &= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VP64_AND_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] &= (o0 | o1 | o2 | o3);
			}
			break;

			case VP64_OR_CR_0:
			{
				regs[(ir >> 8) & 0xf] |= ir >> 12;
			}
			break;

			case VP64_OR_CR_1:
			{
				regs[(ir >> 8) & 0xf] |= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VP64_OR_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] |= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VP64_OR_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] |= (o0 | o1 | o2 | o3);
			}
			break;

			case VP64_XOR_CR_0:
			{
				regs[(ir >> 8) & 0xf] ^= ir >> 12;
			}
			break;

			case VP64_XOR_CR_1:
			{
				regs[(ir >> 8) & 0xf] ^= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VP64_XOR_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] ^= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VP64_XOR_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] ^= (o0 | o1 | o2 | o3);
			}
			break;

			case VP64_SHL_CR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] << (uint64_t)*(uint16_t*)pc++;
			}
			break;

			case VP64_SHR_CR:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)regs[(ir >> 8) & 0xf] >> (uint64_t)*(uint16_t*)pc++;
			}
			break;

			case VP64_ASR_CR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] >> (uint64_t)*(uint16_t*)pc++;
			}
			break;

			case VP64_CPY_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_ADD_RR:
			{
				regs[(ir >> 8) & 0xf] += regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_SUB_RR:
			{
				regs[(ir >> 8) & 0xf] -= regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_CMP_RR:
			{
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_MUL_RR:
			{
				regs[(ir >> 8) & 0xf] *= regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_AND_RR:
			{
				regs[(ir >> 8) & 0xf] &= regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_OR_RR:
			{
				regs[(ir >> 8) & 0xf] |= regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_XOR_RR:
			{
				regs[(ir >> 8) & 0xf] ^= regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_SHL_RR:
			{
				regs[(ir >> 8) & 0xf] <<= regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_SHR_RR:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)regs[(ir >> 8) & 0xf] >> regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_ASR_RR:
			{
				regs[(ir >> 8) & 0xf] >>= regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_LNOT_RR:
			{
				regs[(ir >> 8) & 0xf] = !regs[(ir >> 8) & 0xf];
			}
			break;

			case VP64_LAND_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] && regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_SWP_RR:
			{
				int64_t t = regs[(ir >> 8) & 0xf];
				regs[(ir >> 8) & 0xf] = regs[(ir >> 12) & 0xf];
				regs[(ir >> 12) & 0xf] = t;
			}
			break;

			case VP64_EXT_RR:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 12) & 0xf] >> 63);
			}
			break;

			case VP64_DIV_RRR:
			{
			#ifdef _WIN64
				regs[(ir >> 8) & 0xf] = _div128(regs[(ir >> 12) & 0xf], regs[(ir >> 8) & 0xf], regs[*pc++], &regs[(ir >> 12) & 0xf]);
			#else
				struct i128
				{
					int64_t lo;
					int64_t hi;
				};
				i128 value = {regs[(ir >> 8) & 0xf], regs[(ir >> 12) & 0xf]};
				int64_t div = regs[*pc++];
				regs[(ir >> 8) & 0xf] = (__int128_t&)value / div;
				regs[(ir >> 12) & 0xf] = (__int128_t&)value % div;
			#endif
			}
			break;

			case VP64_DIV_RRR_U:
			{
			#ifdef _WIN64
				regs[(ir >> 8) & 0xf] = _udiv128(regs[(ir >> 12) & 0xf], regs[(ir >> 8) & 0xf], regs[*pc++], (uint64_t*)&regs[(ir >> 12) & 0xf]);
			#else
				struct u128
				{
					uint64_t lo;
					uint64_t hi;
				};
				u128 value = {(uint64_t)regs[(ir >> 8) & 0xf], (uint64_t)regs[(ir >> 12) & 0xf]};
				uint64_t div = regs[*pc++];
				regs[(ir >> 8) & 0xf] = (__uint128_t&)value / div;
				regs[(ir >> 12) & 0xf] = (__uint128_t&)value % div;
			#endif
			}
			break;

			case VP64_SEQ_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] == ir >> 12;
			}
			break;

			case VP64_SEQ_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] == (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VP64_SEQ_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] == (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VP64_SNE_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] != ir >> 12;
			}
			break;

			case VP64_SNE_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] != (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VP64_SNE_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] != (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VP64_SLT_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] < ir >> 12;
			}
			break;

			case VP64_SLT_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] < (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VP64_SLT_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] < (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VP64_SLE_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] <= ir >> 12;
			}
			break;

			case VP64_SLE_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] <= (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VP64_SLE_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] <= (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VP64_SGT_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] > ir >> 12;
			}
			break;

			case VP64_SGT_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] > (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VP64_SGT_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] > (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VP64_SGE_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] >= ir >> 12;
			}
			break;

			case VP64_SGE_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] >= (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VP64_SGE_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] >= (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VP64_SEQ_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] == regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_SNE_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] != regs[(ir >> 12) & 0xf];
			}
			break;
			
			case VP64_SLT_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] < regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_SLE_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] <= regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_SGT_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] > regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_SGE_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] >= regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_BEQ_0:
			{
				if (compare1 == compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_BEQ_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 == compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VP64_BNE_0:
			{
				if (compare1 != compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_BNE_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 != compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VP64_BGE_0:
			{
				if (compare1 >= compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_BGE_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 >= compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VP64_BLT_0:
			{
				if (compare1 < compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_BLT_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 < compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VP64_BLE_0:
			{
				if (compare1 <= compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_BLE_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 <= compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VP64_BGT_0:
			{
				if (compare1 > compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_BGT_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 > compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VP64_CPY_IR_0:
			{
				regs[(ir >> 8) & 0xf] = (int64_t)*(int64_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VP64_CPY_IR_B_0:
			{
				regs[(ir >> 8) & 0xf] = (int64_t)*(int8_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VP64_CPY_IR_S_0:
			{
				regs[(ir >> 8) & 0xf] = (int64_t)*(int16_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VP64_CPY_IR_I_0:
			{
				regs[(ir >> 8) & 0xf] = (int64_t)*(int32_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VP64_CPY_IR_UB_0:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint8_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VP64_CPY_IR_US_0:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint16_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VP64_CPY_IR_UI_0:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint32_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VP64_LEA_I_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 12) & 0xf] + (int64_t)*pc++;
			}
			break;

			case VP64_CPY_RI_0:
			{
				*(int64_t*)(regs[(ir >> 8) & 0xf] + (int64_t)*pc++) = regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_CPY_RI_B_0:
			{
				*(int8_t*)(regs[(ir >> 8) & 0xf] + (int64_t)*pc++) = (int8_t)regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_CPY_RI_S_0:
			{
				*(int16_t*)(regs[(ir >> 8) & 0xf] + (int64_t)*pc++) = (int16_t)regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_CPY_RI_I_0:
			{
				*(int32_t*)(regs[(ir >> 8) & 0xf] + (int64_t)*pc++) = (int32_t)regs[(ir >> 12) & 0xf];
			}
			break;

			case VP64_CPY_RD:
			{
				*(uint64_t*)(regs[(ir >> 8) & 0xf] + regs[(ir >> 12) & 0xf]) = regs[*pc++];
			}
			break;

			case VP64_CPY_RD_B:
			{
				*(uint8_t*)(regs[(ir >> 8) & 0xf] + regs[(ir >> 12) & 0xf]) = (uint8_t)regs[*pc++];
			}
			break;

			case VP64_CPY_RD_S:
			{
				*(uint16_t*)(regs[(ir >> 8) & 0xf] + regs[(ir >> 12) & 0xf]) = (uint16_t)regs[*pc++];
			}
			break;

			case VP64_CPY_RD_I:
			{
				*(uint32_t*)(regs[(ir >> 8) & 0xf] + regs[(ir >> 12) & 0xf]) = (uint32_t)regs[*pc++];
			}
			break;

			case VP64_CPY_DR:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(int64_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VP64_CPY_DR_B:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(int8_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VP64_CPY_DR_S:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(int16_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VP64_CPY_DR_I:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(int32_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VP64_CPY_DR_UB:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint8_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VP64_CPY_DR_US:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint16_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VP64_CPY_DR_UI:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint32_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VP64_LEA_D:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 12) & 0xf] + regs[*pc++];
			}
			break;

			case VP64_CALL_0:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc;
				pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_CALL_1:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc + 1;
				int64_t o = ((int64_t)*pc++ << 8) + ((ir >> 8) & 0xff);
				pc = (int16_t*)((char*)pc + o);
			}
			break;

			case VP64_JMP_0:
			{
				pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_JMP_1:
			{
				int64_t o = ((int64_t)*pc++ << 8) + ((ir >> 8) & 0xff);
				pc = (int16_t*)((char*)pc + o);
			}
			break;

			case VP64_CALL_P_0:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc;
				pc = *(int16_t**)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_CALL_P_1:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc+1;
				int64_t o = ((int64_t)*pc++ << 8) + ((ir >> 8) & 0xff);
				pc = *(int16_t**)((char*)pc + o);
			}
			break;

			case VP64_JMP_P_0:
			{
				pc = *(int16_t**)((char*)pc + (ir >> 8));
			}
			break;

			case VP64_JMP_P_1:
			{
				int64_t o = ((int64_t)*pc++ << 8) + ((ir >> 8) & 0xff);
				pc = *(int16_t**)((char*)pc + o);
			}
			break;

			case VP64_CALL_ABI:
			{
				switch ((ir >> 12) & 0xf)
				{
					case 0:
					{
						typedef uint64_t(*FUNCPTR)(void);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)();
					}
					break;

					case 1:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0]);
					}
					break;

					case 2:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1]);
					}
					break;

					case 3:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2]);
					}
					break;

					case 4:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3]);
					}
					break;

					case 5:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4]);
					}
					break;

					case 6:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
										regs[5]);
					}
					break;

					case 7:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
										  regs[5], regs[6]);
					}
					break;

					case 8:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7]);
					}
					break;

					case 9:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t,uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8]);
					}
					break;

					case 10:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t,uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9]);
					}
					break;

					case 11:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10]);
					}
					break;

					case 12:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10], regs[11]);
					}
					break;

					case 13:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10], regs[11], regs[12]);
					}
					break;

					case 14:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10], regs[11], regs[12], regs[13]);
					}
					break;

					case 15:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
												   uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR)*(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));
						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10], regs[11], regs[12], regs[13], regs[14]);
					}
					break;
				}
			}
			break;

			case VP64_CALL_R:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc;
				pc = (int16_t*)regs[(ir >> 8) & 0xf];			
			}
			break;

			case VP64_JMP_R:
			{
				pc = (int16_t*)regs[(ir >> 8) & 0xf];
			}
			break;

			case VP64_CALL_I:
			{
				int64_t base = regs[(ir >> 8) & 0xf];
				int64_t o = ((int64_t)*pc++ << 4) + ((ir >> 12) & 0xf);
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc;
				pc = *(int16_t**)(base + o);
			}
			break;

			case VP64_JMP_I:
			{
				int64_t o = ((int64_t)*pc++ << 4) + ((ir >> 12) & 0xf);
				pc = *(int16_t**)(regs[(ir >> 8) & 0xf] + o);
			}
			break;

			case VP64_CPY_PR:
			{
				int64_t o = ((int64_t)*pc++ << 4) + ((ir >> 12) & 0xf);
				regs[(ir >> 8) & 0xf] = *(int64_t*)((char*)pc + o);
			}
			break;

			case VP64_LEA_P:
			{
				int64_t o = ((int64_t)*pc++ << 4) + ((ir >> 12) & 0xf);
				regs[(ir >> 8) & 0xf] = (int64_t)((char*)pc + o);
			}
			break;

			case VP64_RET:
			{
				pc = *(int16_t**)regs[15];
				regs[15] += 8;
			}
			break;

			case VP64_SYNC:
			{
				sync.store(ir >> 8, std::memory_order_seq_cst);
			}
			break;

			case VP64_BRK:
			{
				std::cout << "brk " << ((ir >> 8) & 0xff) << std::endl;
			}
			break;

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
