//////////////////////////////////////
// ChrysaLisp pii interface structures
//////////////////////////////////////

#ifndef PII_H
#define PII_H

#include <inttypes.h>

enum
{
	mmap_data,
	mmap_exec,
	mmap_shared,
	mmap_none
};

enum
{
	file_open_read,
	file_open_write,
	file_open_append
};

struct pii_stat_info
{
	int64_t mtime;
	int64_t fsize;
	uint16_t mode;
};

struct fn_header
{
	uint64_t ln_fnode;
	uint16_t length;
	uint16_t entry;
	uint16_t links;
	uint16_t paths;
	uint16_t stack;
	uint16_t pathname;
};

#endif
