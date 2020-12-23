#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <random>
#ifdef _WIN64
#define _CRT_SECURE_NO_WARNINGS
#define DELTA_EPOCH_IN_MICROSECS 11644473600000000Ui64
#include <time.h>
#include <io.h>
#include <windows.h>
#include <tchar.h>
#include <direct.h>
#include <conio.h>

#include <immintrin.h>
#else
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
#include <dirent.h>
#endif

#include <string>
#include <iostream>

using namespace std;

#pragma pack(1)
struct FuncHeader
{
	uint64_t	ln_fnode;
	uint16_t	fn_header_length;
	uint16_t	fn_header_entry;
	uint16_t	fn_header_links;
	uint16_t	fn_header_paths;
	uint16_t	fn_header_stack;
	uint16_t	fn_header_pathname;
};

enum Opcodes
{
	VMOP_CPY_CR_0,
	VMOP_CPY_CR_1,
	VMOP_CPY_CR_2,
	VMOP_CPY_CR_3,
	VMOP_ADD_CR_0,
	VMOP_ADD_CR_1,
	VMOP_ADD_CR_2,
	VMOP_ADD_CR_3,
	VMOP_SUB_CR_0,
	VMOP_SUB_CR_1,
	VMOP_SUB_CR_2,
	VMOP_SUB_CR_3,
	VMOP_CMP_CR_0,
	VMOP_CMP_CR_1,
	VMOP_CMP_CR_2,
	VMOP_CMP_CR_3,
	VMOP_MUL_CR_0,
	VMOP_MUL_CR_1,
	VMOP_MUL_CR_2,
	VMOP_MUL_CR_3,
	VMOP_AND_CR_0,
	VMOP_AND_CR_1,
	VMOP_AND_CR_2,
	VMOP_AND_CR_3,
	VMOP_OR_CR_0,
	VMOP_OR_CR_1,
	VMOP_OR_CR_2,
	VMOP_OR_CR_3,
	VMOP_XOR_CR_0,
	VMOP_XOR_CR_1,
	VMOP_XOR_CR_2,
	VMOP_XOR_CR_3,

	VMOP_SHL_CR,
	VMOP_SHR_CR,
	VMOP_ASR_CR,
	
	VMOP_CPY_RR,
	VMOP_ADD_RR,
	VMOP_SUB_RR,
	VMOP_CMP_RR,
	VMOP_MUL_RR,
	VMOP_AND_RR,
	VMOP_OR_RR,
	VMOP_XOR_RR,
	VMOP_SHL_RR,
	VMOP_SHR_RR,
	VMOP_ASR_RR,
	VMOP_LNOT_RR,
	VMOP_LAND_RR,
	VMOP_SWP_RR,
	VMOP_EXT_RR,
	VMOP_DIV_RRR,
	VMOP_DIV_RRR_U,

	VMOP_SEQ_CR_0,
	VMOP_SEQ_CR_1,
	VMOP_SEQ_CR_2,
	
	VMOP_SNE_CR_0,
	VMOP_SNE_CR_1,
	VMOP_SNE_CR_2,
	
	VMOP_SLT_CR_0,
	VMOP_SLT_CR_1,
	VMOP_SLT_CR_2,
	
	VMOP_SLE_CR_0,
	VMOP_SLE_CR_1,
	VMOP_SLE_CR_2,
	
	VMOP_SGT_CR_0,
	VMOP_SGT_CR_1,
	VMOP_SGT_CR_2,
	
	VMOP_SGE_CR_0,
	VMOP_SGE_CR_1,
	VMOP_SGE_CR_2,
	
	VMOP_SEQ_RR,
	VMOP_SNE_RR,
	VMOP_SLT_RR,
	VMOP_SLE_RR,
	VMOP_SGT_RR,
	VMOP_SGE_RR,

	VMOP_BEQ_0,
	VMOP_BEQ_1,
	VMOP_BNE_0,
	VMOP_BNE_1,
	VMOP_BGE_0,
	VMOP_BGE_1,
	VMOP_BLT_0,
	VMOP_BLT_1,
	VMOP_BLE_0,
	VMOP_BLE_1,
	VMOP_BGT_0,
	VMOP_BGT_1,

	VMOP_CPY_IR_0,
	VMOP_CPY_IR_B_0,
	VMOP_CPY_IR_S_0,
	VMOP_CPY_IR_I_0,
	VMOP_CPY_IR_UB_0,
	VMOP_CPY_IR_US_0,
	VMOP_CPY_IR_UI_0,
	VMOP_LEA_I_0,

	VMOP_CPY_RI_0,
	VMOP_CPY_RI_B_0,
	VMOP_CPY_RI_S_0,
	VMOP_CPY_RI_I_0,

	VMOP_CPY_RD,
	VMOP_CPY_RD_B,
	VMOP_CPY_RD_S,
	VMOP_CPY_RD_I,

	VMOP_CPY_DR,
	VMOP_CPY_DR_B,
	VMOP_CPY_DR_S,
	VMOP_CPY_DR_I,
	VMOP_CPY_DR_UB,
	VMOP_CPY_DR_US,
	VMOP_CPY_DR_UI,
	VMOP_LEA_D,

	VMOP_CALL_R,
	VMOP_JMP_R,

	VMOP_CALL_I,
	VMOP_JMP_I,

	VMOP_CPY_RP,
	VMOP_CPY_PR,
	VMOP_LEA_P,

	VMOP_CALL_0,
	VMOP_CALL_1,
	VMOP_JMP_0,
	VMOP_JMP_1,

	VMOP_CALL_P_0,
	VMOP_CALL_P_1,
	VMOP_JMP_P_0,
	VMOP_JMP_P_1,

	VMOP_CALL_ABI,

	VMOP_RET,
	VMOP_BRK,
};

std::string opcodeDesc[] = {
	"VMOP_CPY_CR_0",
	"VMOP_CPY_CR_1",
	"VMOP_CPY_CR_2",
	"VMOP_CPY_CR_3",
	"VMOP_ADD_CR_0",
	"VMOP_ADD_CR_1",
	"VMOP_ADD_CR_2",
	"VMOP_ADD_CR_3",
	"VMOP_SUB_CR_0",
	"VMOP_SUB_CR_1",
	"VMOP_SUB_CR_2",
	"VMOP_SUB_CR_3",
	"VMOP_CMP_CR_0",
	"VMOP_CMP_CR_1",
	"VMOP_CMP_CR_2",
	"VMOP_CMP_CR_3",
	"VMOP_MUL_CR_0",
	"VMOP_MUL_CR_1",
	"VMOP_MUL_CR_2",
	"VMOP_MUL_CR_3",
	"VMOP_AND_CR_0",
	"VMOP_AND_CR_1",
	"VMOP_AND_CR_2",
	"VMOP_AND_CR_3",
	"VMOP_OR_CR_0",
	"VMOP_OR_CR_1",
	"VMOP_OR_CR_2",
	"VMOP_OR_CR_3",
	"VMOP_XOR_CR_0",
	"VMOP_XOR_CR_1",
	"VMOP_XOR_CR_2",
	"VMOP_XOR_CR_3",

	"VMOP_SHL_CR",
	"VMOP_SHR_CR",
	"VMOP_ASR_CR",

	"VMOP_CPY_RR",
	"VMOP_ADD_RR",
	"VMOP_SUB_RR",
	"VMOP_CMP_RR",
	"VMOP_MUL_RR",
	"VMOP_AND_RR",
	"VMOP_OR_RR",
	"VMOP_XOR_RR",
	"VMOP_SHL_RR",
	"VMOP_SHR_RR",
	"VMOP_ASR_RR",
	"VMOP_LNOT_RR",
	"VMOP_LAND_RR",
	"VMOP_SWP_RR",
	"VMOP_EXT_RR",
	"VMOP_DIV_RRR",
	"VMOP_DIV_RRR_U",

	"VMOP_SEQ_CR_0",
	"VMOP_SEQ_CR_1",
	"VMOP_SEQ_CR_2",

	"VMOP_SNE_CR_0",
	"VMOP_SNE_CR_1",
	"VMOP_SNE_CR_2",

	"VMOP_SLT_CR_0",
	"VMOP_SLT_CR_1",
	"VMOP_SLT_CR_2",

	"VMOP_SLE_CR_0",
	"VMOP_SLE_CR_1",
	"VMOP_SLE_CR_2",

	"VMOP_SGT_CR_0",
	"VMOP_SGT_CR_1",
	"VMOP_SGT_CR_2",

	"VMOP_SGE_CR_0",
	"VMOP_SGE_CR_1",
	"VMOP_SGE_CR_2",

	"VMOP_SEQ_RR",
	"VMOP_SNE_RR",
	"VMOP_SLT_RR",
	"VMOP_SLE_RR",
	"VMOP_SGT_RR",
	"VMOP_SGE_RR",

	"VMOP_BEQ_0",
	"VMOP_BEQ_1",
	"VMOP_BNE_0",
	"VMOP_BNE_1",
	"VMOP_BGE_0",
	"VMOP_BGE_1",
	"VMOP_BLT_0",
	"VMOP_BLT_1",
	"VMOP_BLE_0",
	"VMOP_BLE_1",
	"VMOP_BGT_0",
	"VMOP_BGT_1",

	"VMOP_CPY_IR_0",
	"VMOP_CPY_IR_B_0",
	"VMOP_CPY_IR_S_0",
	"VMOP_CPY_IR_I_0",
	"VMOP_CPY_IR_UB_0",
	"VMOP_CPY_IR_US_0",
	"VMOP_CPY_IR_UI_0",
	"VMOP_LEA_I_0",

	"VMOP_CPY_RI_0",
	"VMOP_CPY_RI_B_0",
	"VMOP_CPY_RI_S_0",
	"VMOP_CPY_RI_I_0",

	"VMOP_CPY_RD",
	"VMOP_CPY_RD_B",
	"VMOP_CPY_RD_S",
	"VMOP_CPY_RD_I",

	"VMOP_CPY_DR",
	"VMOP_CPY_DR_B",
	"VMOP_CPY_DR_S",
	"VMOP_CPY_DR_I",
	"VMOP_CPY_DR_UB",
	"VMOP_CPY_DR_US",
	"VMOP_CPY_DR_UI",
	"VMOP_LEA_D",

	"VMOP_CALL_R",
	"VMOP_JMP_R",

	"VMOP_CALL_I",
	"VMOP_JMP_I",

	"VMOP_CPY_RP",
	"VMOP_CPY_PR",
	"VMOP_LEA_P",

	"VMOP_CALL_0",
	"VMOP_CALL_1",
	"VMOP_JMP_0",
	"VMOP_JMP_1",

	"VMOP_CALL_P_0",
	"VMOP_CALL_P_1",
	"VMOP_JMP_P_0",
	"VMOP_JMP_P_1",

	"VMOP_CALL_ABI",

	"VMOP_RET",
	"VMOP_BRK"
};

int vp64(uint8_t* data, int64_t *stack, int64_t* argv, int64_t* host_funcs)
{
	int64_t regs[16];
	register int16_t* pc;
	register int64_t ir;
	register int64_t compare1;
	register int64_t compare2;

	// start at the beginning
	FuncHeader* pHeader = (FuncHeader*)((uint8_t*)data);
	pc = (int16_t*)((uint8_t*)data + pHeader->fn_header_entry);
	regs[0] = (uint64_t)argv;
	regs[1] = (uint64_t)host_funcs;
	regs[15] = (uint64_t)stack;

	for(;;)
	{
		ir = *pc++;
		switch (ir & 0xff)
		{
			case VMOP_CPY_CR_0:
			{
				regs[(ir >> 8) & 0xf] = ir >> 12;
			}
			break;

			case VMOP_CPY_CR_1:
			{
				regs[(ir >> 8) & 0xf] = ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VMOP_CPY_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VMOP_CPY_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] = (o0 | o1 | o2 | o3);
			}
			break;
		
			case VMOP_ADD_CR_0:
			{
				regs[(ir >> 8) & 0xf] += ir >> 12;
			}
			break;

			case VMOP_ADD_CR_1:
			{
				regs[(ir >> 8) & 0xf] += ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VMOP_ADD_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] += ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VMOP_ADD_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] += (o0 | o1 | o2 | o3);
			}
			break;
		
			case VMOP_SUB_CR_0:
			{
				regs[(ir >> 8) & 0xf] -= ir >> 12;
			}
			break;

			case VMOP_SUB_CR_1:
			{
				regs[(ir >> 8) & 0xf] -= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VMOP_SUB_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] -= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VMOP_SUB_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] -= (o0 | o1 | o2 | o3);
			}
			break;

			case VMOP_CMP_CR_0:
			{
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = ir >> 12;
			}
			break;

			case VMOP_CMP_CR_1:
			{
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VMOP_CMP_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VMOP_CMP_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*(uint16_t*)pc++ << 48;
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = (o0 | o1 | o2 | o3);
			}
			break;

			case VMOP_MUL_CR_0:
			{
				regs[(ir >> 8) & 0xf] *= ir >> 12;
			}
			break;

			case VMOP_MUL_CR_1:
			{
				regs[(ir >> 8) & 0xf] *= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VMOP_MUL_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] *= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VMOP_MUL_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] *= (o0 | o1 | o2 | o3);
			}
			break;

			case VMOP_AND_CR_0:
			{
				regs[(ir >> 8) & 0xf] &= ir >> 12;
			}
			break;

			case VMOP_AND_CR_1:
			{
				regs[(ir >> 8) & 0xf] &= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VMOP_AND_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] &= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VMOP_AND_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] &= (o0 | o1 | o2 | o3);
			}
			break;

			case VMOP_OR_CR_0:
			{
				regs[(ir >> 8) & 0xf] |= ir >> 12;
			}
			break;

			case VMOP_OR_CR_1:
			{
				regs[(ir >> 8) & 0xf] |= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VMOP_OR_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] |= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VMOP_OR_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] |= (o0 | o1 | o2 | o3);
			}
			break;

			case VMOP_XOR_CR_0:
			{
				regs[(ir >> 8) & 0xf] ^= ir >> 12;
			}
			break;

			case VMOP_XOR_CR_1:
			{
				regs[(ir >> 8) & 0xf] ^= ((ir >> 12) & 0xf) | ((int64_t)*pc++ << 4);
			}
			break;

			case VMOP_XOR_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] ^= ((ir >> 12) & 0xf) | o1 | o2;
			}
			break;

			case VMOP_XOR_CR_3:
			{
				uint64_t o0 = (uint64_t)*(uint16_t*)pc++;
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 16;
				uint64_t o2 = (uint64_t)*(uint16_t*)pc++ << 32;
				int64_t o3 = (int64_t)*pc++ << 48;
				regs[(ir >> 8) & 0xf] ^= (o0 | o1 | o2 | o3);
			}
			break;

			case VMOP_SHL_CR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] << (uint64_t)*(uint16_t*)pc++;
			}
			break;

			case VMOP_SHR_CR:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)regs[(ir >> 8) & 0xf] >> (uint64_t)*(uint16_t*)pc++;
			}
			break;

			case VMOP_ASR_CR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] >> (uint64_t)*(uint16_t*)pc++;
			}
			break;

			case VMOP_CPY_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_ADD_RR:
			{
				regs[(ir >> 8) & 0xf] += regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_SUB_RR:
			{
				regs[(ir >> 8) & 0xf] -= regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_CMP_RR:
			{
				compare1 = regs[(ir >> 8) & 0xf];
				compare2 = regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_MUL_RR:
			{
				regs[(ir >> 8) & 0xf] *= regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_AND_RR:
			{
				regs[(ir >> 8) & 0xf] &= regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_OR_RR:
			{
				regs[(ir >> 8) & 0xf] |= regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_XOR_RR:
			{
				regs[(ir >> 8) & 0xf] ^= regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_SHL_RR:
			{
				regs[(ir >> 8) & 0xf] <<= regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_SHR_RR:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)regs[(ir >> 8) & 0xf] >> regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_ASR_RR:
			{
				regs[(ir >> 8) & 0xf] >>= regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_LNOT_RR:
			{
				regs[(ir >> 8) & 0xf] = !regs[(ir >> 8) & 0xf];
			}
			break;

			case VMOP_LAND_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] && regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_SWP_RR:
			{
				int64_t t = regs[(ir >> 8) & 0xf];
				regs[(ir >> 8) & 0xf] = regs[(ir >> 12) & 0xf];
				regs[(ir >> 12) & 0xf] = t;
			}
			break;

			case VMOP_EXT_RR:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 12) & 0xf] >> 63);
			}
			break;

			case VMOP_DIV_RRR:
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

			case VMOP_DIV_RRR_U:
			{
#ifdef _WIN64
				regs[(ir >> 8) & 0xf] = _udiv128(regs[(ir >> 12) & 0xf], regs[(ir >> 8) & 0xf], regs[*pc++], (uint64_t*)&regs[(ir >> 12) & 0xf]);
#else
				struct u128
				{
					uint64_t lo;
					uint64_t hi;
				};
				u128 value = {regs[(ir >> 8) & 0xf], regs[(ir >> 12) & 0xf]};
				uint64_t div = regs[*pc++];
				regs[(ir >> 8) & 0xf] = (__uint128_t&)value / div;
				regs[(ir >> 12) & 0xf] = (__uint128_t&)value % div;
#endif
			}
			break;

			case VMOP_SEQ_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] == ir >> 12;
			}
			break;

			case VMOP_SEQ_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] == (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VMOP_SEQ_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] == (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VMOP_SNE_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] != ir >> 12;
			}
			break;

			case VMOP_SNE_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] != (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VMOP_SNE_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] != (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VMOP_SLT_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] < ir >> 12;
			}
			break;

			case VMOP_SLT_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] < (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VMOP_SLT_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] < (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VMOP_SLE_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] <= ir >> 12;
			}
			break;

			case VMOP_SLE_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] <= (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VMOP_SLE_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] <= (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VMOP_SGT_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] > ir >> 12;
			}
			break;

			case VMOP_SGT_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] > (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VMOP_SGT_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] > (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VMOP_SGE_CR_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] >= ir >> 12;
			}
			break;

			case VMOP_SGE_CR_1:
			{
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] >= (((ir >> 12) & 0xf) |
					((int64_t)*pc++ << 4)));
			}
			break;

			case VMOP_SGE_CR_2:
			{
				uint64_t o1 = (uint64_t)*(uint16_t*)pc++ << 4;
				int64_t o2 = (int64_t)*pc++ << 20;
				regs[(ir >> 8) & 0xf] = (regs[(ir >> 8) & 0xf] >= (((ir >> 12) & 0xf) | o1 | o2));
			}
			break;

			case VMOP_SEQ_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] == regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_SNE_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] != regs[(ir >> 12) & 0xf];
			}
			break;
			
			case VMOP_SLT_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] < regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_SLE_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] <= regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_SGT_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] > regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_SGE_RR:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 8) & 0xf] >= regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_BEQ_0:
			{
				if (compare1 == compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_BEQ_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 == compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VMOP_BNE_0:
			{
				if (compare1 != compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_BNE_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 != compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VMOP_BGE_0:
			{
				if (compare1 >= compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_BGE_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 >= compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VMOP_BLT_0:
			{
				if (compare1 < compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_BLT_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 < compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VMOP_BLE_0:
			{
				if (compare1 <= compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_BLE_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 <= compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VMOP_BGT_0:
			{
				if (compare1 > compare2) pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_BGT_1:
			{
				int64_t offset = ((ir >> 8) & 0xff) | (int64_t)*pc++ << 8;
				if (compare1 > compare2) pc = (int16_t*)((char*)pc + offset);
			}
			break;

			case VMOP_CPY_IR_0:
			{
				regs[(ir >> 8) & 0xf] = (int64_t)*(int64_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VMOP_CPY_IR_B_0:
			{
				regs[(ir >> 8) & 0xf] = (int64_t)*(int8_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VMOP_CPY_IR_S_0:
			{
				regs[(ir >> 8) & 0xf] = (int64_t)*(int16_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VMOP_CPY_IR_I_0:
			{
				regs[(ir >> 8) & 0xf] = (int64_t)*(int32_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VMOP_CPY_IR_UB_0:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint8_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VMOP_CPY_IR_US_0:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint16_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VMOP_CPY_IR_UI_0:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint32_t*)(regs[(ir >> 12) & 0xf] + (int64_t)*pc++);
			}
			break;

			case VMOP_LEA_I_0:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 12) & 0xf] + (int64_t)*pc++;
			}
			break;

			case VMOP_CPY_RI_0:
			{
				*(int64_t*)(regs[(ir >> 8) & 0xf] + (int64_t)*pc++) = regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_CPY_RI_B_0:
			{
				*(int8_t*)(regs[(ir >> 8) & 0xf] + (int64_t)*pc++) = regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_CPY_RI_S_0:
			{
				*(int16_t*)(regs[(ir >> 8) & 0xf] + (int64_t)*pc++) = regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_CPY_RI_I_0:
			{
				*(int32_t*)(regs[(ir >> 8) & 0xf] + (int64_t)*pc++) = regs[(ir >> 12) & 0xf];
			}
			break;

			case VMOP_CPY_RD:
			{
				*(uint64_t*)(regs[(ir >> 8) & 0xf] + regs[(ir >> 12) & 0xf]) = regs[*pc++];
			}
			break;

			case VMOP_CPY_RD_B:
			{
				*(uint8_t*)(regs[(ir >> 8) & 0xf] + regs[(ir >> 12) & 0xf]) = regs[*pc++];
			}
			break;

			case VMOP_CPY_RD_S:
			{
				*(uint16_t*)(regs[(ir >> 8) & 0xf] + regs[(ir >> 12) & 0xf]) = regs[*pc++];
			}
			break;

			case VMOP_CPY_RD_I:
			{
				*(uint32_t*)(regs[(ir >> 8) & 0xf] + regs[(ir >> 12) & 0xf]) = regs[*pc++];
			}
			break;

			case VMOP_CPY_DR:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(int64_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VMOP_CPY_DR_B:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(int8_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VMOP_CPY_DR_S:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(int16_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VMOP_CPY_DR_I:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(int32_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VMOP_CPY_DR_UB:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint8_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VMOP_CPY_DR_US:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint16_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VMOP_CPY_DR_UI:
			{
				regs[(ir >> 8) & 0xf] = (uint64_t)*(uint32_t*)(regs[(ir >> 12) & 0xf] + regs[*pc++]);
			}
			break;

			case VMOP_LEA_D:
			{
				regs[(ir >> 8) & 0xf] = regs[(ir >> 12) & 0xf] + regs[*pc++];
			}
			break;

			case VMOP_CALL_0:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc;
				pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_CALL_1:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc + 1;
				int64_t o = ((int64_t)*pc++ << 8) + ((ir >> 8) & 0xff);
				pc = (int16_t*)((char*)pc + o);
			}
			break;

			case VMOP_JMP_0:
			{
				pc = (int16_t*)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_JMP_1:
			{
				int64_t o = ((int64_t)*pc++ << 8) + ((ir >> 8) & 0xff);
				pc = (int16_t*)((char*)pc + o);
			}
			break;

			case VMOP_CALL_P_0:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc;
				pc = *(int16_t**)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_CALL_P_1:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc+1;
				int64_t o = ((int64_t)*pc++ << 8) + ((ir >> 8) & 0xff);
				pc = *(int16_t**)((char*)pc + o);
			}
			break;

			case VMOP_JMP_P_0:
			{
				pc = *(int16_t**)((char*)pc + (ir >> 8));
			}
			break;

			case VMOP_JMP_P_1:
			{
				int64_t o = ((int64_t)*pc++ << 8) + ((ir >> 8) & 0xff);
				pc = *(int16_t**)((char*)pc + o);
			}
			break;

			case VMOP_CALL_ABI:
			{
				switch ((ir >> 12) & 0xf)
				{
					case 0:
					{
						typedef uint64_t(*FUNCPTR)(void);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)();
					}
					break;

					case 1:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0]);
					}
					break;

					case 2:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1]);
					}
					break;

					case 3:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2]);
					}
					break;

					case 4:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3]);
					}
					break;

					case 5:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4]);
					}
					break;

					case 6:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							              regs[5]);
					}
					break;

					case 7:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
										  regs[5], regs[6]);
					}
					break;

					case 8:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
													uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7]);
					}
					break;

					case 9:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
							uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8]);
					}
					break;

					case 10:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
							uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9]);
					}
					break;

					case 11:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
							uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10]);
					}
					break;

					case 12:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
							uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10], regs[11]);
					}
					break;

					case 13:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
							uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10], regs[11], regs[12]);
					}
					break;

					case 14:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
							uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10], regs[11], regs[12], regs[13]);
					}
					break;

					case 15:
					{
						typedef uint64_t(*FUNCPTR)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
							uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
						FUNCPTR fptr = (FUNCPTR) *(uint64_t*)(regs[(ir >> 8) & 0xf] + ((uint64_t)*(uint16_t*)pc++));

						regs[0] = (*fptr)(regs[0], regs[1], regs[2], regs[3], regs[4],
							regs[5], regs[6], regs[7], regs[8], regs[9],
							regs[10], regs[11], regs[12], regs[13], regs[14]);
					}
					break;
				}
			}
			break;

			case VMOP_CALL_R:
			{
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc;
				pc = (int16_t*)regs[(ir >> 8) & 0xf];			
			}
			break;

			case VMOP_JMP_R:
			{
				pc = (int16_t*)regs[(ir >> 8) & 0xf];
			}
			break;

			case VMOP_CALL_I:
			{
				int64_t base = regs[(ir >> 8) & 0xf];
				int64_t o = ((int64_t)*pc++ << 4) + ((ir >> 12) & 0xf);
				regs[15] -= 8;
				*(int16_t**)regs[15] = pc;
				pc = *(int16_t**)(base + o);
			}
			break;

			case VMOP_JMP_I:
			{
				int64_t o = ((int64_t)*pc++ << 4) + ((ir >> 12) & 0xf);
				pc = *(int16_t**)(regs[(ir >> 8) & 0xf] + o);
			}
			break;

			case VMOP_CPY_RP:
			{
				int64_t o = ((int64_t)*pc++ << 4) + ((ir >> 12) & 0xf);
				*(int64_t*)((char*)pc + o) = regs[(ir >> 8) & 0xf];
			}
			break;
				
			case VMOP_CPY_PR:
			{
				int64_t o = ((int64_t)*pc++ << 4) + ((ir >> 12) & 0xf);
				regs[(ir >> 8) & 0xf] = *(int64_t*)((char*)pc + o);
			}
			break;

			case VMOP_LEA_P:
			{
				int64_t o = ((int64_t)*pc++ << 4) + ((ir >> 12) & 0xf);
				regs[(ir >> 8) & 0xf] = (int64_t)((char*)pc + o);
			}
			break;

			case VMOP_RET:
			{
				pc = *(int16_t**)regs[15];
				regs[15] += 8;
			}
			break;

			case VMOP_BRK:
			{
				cout << "brk " << ((ir >> 8) & 0xff) << endl;
			}
			break;

			default:
			{
				cout << "Unrecognised opcode " << (ir & 0xff) << " " << endl;
				break;
			}
			break;
		}
	}
	return 0;
}
