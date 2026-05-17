//////////////////////////////////////
// ChrysaLisp pii interface structures
//////////////////////////////////////

#ifndef PII_H
#define PII_H

#include <inttypes.h>
#include <stddef.h>

//hard values for now matching sys/link/class.inc
const uint32_t lk_page_size = 4096;
const uint32_t lk_data_size = 4056;
const uint32_t lk_bufs_per_chan = 6;

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

struct alignas(8) node_id
{
	uint64_t m_node1;
	uint64_t m_node2;
};

struct alignas(8) net_id
{
	uint64_t m_mbox_id;
	node_id m_node_id;
};

// Continuous ring buffer status types
enum
{
	lk_chan_status_ready,
	lk_chan_status_ping,
	lk_chan_status_frag,
	lk_chan_status_skip
};

// Base transfer unit header
struct alignas(8) lk_buf
{
	uint32_t m_status;
	uint32_t m_length;
};

// Peer info payload for ping status
struct alignas(8) lk_node
{
	node_id m_peer_node_id;
	uint32_t m_task_count;
};

// Fragment header payload for frag status
struct alignas(8) lk_frag
{
	net_id m_dest;
	net_id m_src;
	uint32_t m_length;
	uint32_t m_offset;
	uint32_t m_total;
};

const uint32_t lk_chan_size = (sizeof(lk_buf) + sizeof(lk_frag) + lk_data_size) * lk_bufs_per_chan;

struct lk_chan
{
	char m_data[lk_chan_size];
};

struct lk_shmem
{
	lk_chan m_chan_1;
	// Negotiation "Towel" embedded in the alignment padding of chan_1
	node_id m_host_a;
	node_id m_host_b;
	alignas(lk_page_size) lk_chan m_chan_2;
};

static_assert(sizeof(lk_shmem) % lk_page_size == 0, "lk_shmem size must be a multiple of lk_page_size");
static_assert(offsetof(lk_shmem, m_chan_2) % lk_page_size == 0, "lk_shmem::m_chan_2 must be page aligned");

#endif