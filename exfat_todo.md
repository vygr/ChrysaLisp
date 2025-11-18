# ExFat Filesystem - Future Utilities TODO

This document tracks planned ExFat utilities that are deferred for future implementation.

## Deferred Utilities

### 4. exfatdefrag - Defragmentation Tool

**Purpose**: Analyze and optimize cluster allocation patterns

**Planned Features**:
- Scan filesystem for fragmented files
- Calculate overall fragmentation percentage
- Identify most fragmented files (by fragment count)
- Show fragmentation statistics per file
- Report average fragments per file
- Suggest defragmentation strategy
- (Optional) Perform actual defragmentation by relocating clusters

**Use Cases**:
- Performance optimization
- Understanding allocation patterns
- Identifying problematic files
- Benchmarking before/after optimization

**Implementation Complexity**: Medium
- Requires: FAT chain following, file metadata reading
- Analysis: Traverse all files, count non-sequential clusters
- Optimization (if implemented): Safe cluster relocation, FAT updates

**API Design**:
```lisp
; Analysis only (read-only)
(defun analyze_fragmentation (exfat_obj)
  ; Returns: (total_files fragmented_files avg_fragments max_fragments)
  )

; Per-file analysis
(defun get_file_fragmentation (exfat_obj file_path)
  ; Returns: (fragment_count clusters is_sequential)
  )

; Report generation
(defun fragmentation_report (exfat_obj)
  ; Prints detailed fragmentation report
  )
```

**Test Coverage Needed**:
- Detect sequential files (0 fragments)
- Detect fragmented files (multiple fragments)
- Calculate percentages correctly
- Handle empty filesystem
- Handle single-file filesystem
- Handle large files with many fragments

**Estimated Effort**: 2-3 hours implementation + 1 hour tests

---

### 5. exfatbench - Performance Benchmark Tool

**Purpose**: Measure filesystem performance characteristics

**Planned Features**:
- Sequential read/write speed (large blocks)
- Random access patterns (small blocks)
- Cluster allocation speed
- FAT update performance
- Directory traversal speed
- Compare different filesystem configurations
- Generate performance reports with graphs (ASCII art)

**Use Cases**:
- Performance analysis
- Configuration optimization
- Regression testing
- Comparing stream implementations (memory-stream vs file-stream)

**Implementation Complexity**: Medium-High
- Requires: Timing infrastructure, various access patterns
- Statistics: Throughput (MB/s), IOPS, latency percentiles
- Reporting: ASCII graphs, percentile distributions

**Benchmark Types**:

1. **Sequential Performance**:
   - Write large file sequentially
   - Read large file sequentially
   - Measure throughput (MB/s)

2. **Random Access**:
   - Random cluster reads
   - Random cluster writes
   - Measure IOPS (operations per second)

3. **Allocation Performance**:
   - Allocate N clusters sequentially
   - Allocate N clusters with gaps
   - Measure allocations per second

4. **FAT Operations**:
   - Read FAT entries (sequential)
   - Write FAT entries (sequential)
   - Random FAT access

5. **Metadata Operations**:
   - Create N files
   - List directory with N entries
   - Delete N files

**API Design**:
```lisp
(defun benchmark_sequential_write (exfat_obj size_mb)
  ; Returns: (mb_per_second duration_ms)
  )

(defun benchmark_random_access (exfat_obj operations)
  ; Returns: (iops avg_latency_us)
  )

(defun benchmark_allocation (exfat_obj cluster_count)
  ; Returns: (allocations_per_second)
  )

(defun run_full_benchmark (exfat_obj)
  ; Runs all benchmarks and prints report
  )
```

**Test Coverage Needed**:
- Verify benchmark completes without errors
- Validate timing measurements are reasonable
- Test with various filesystem sizes
- Verify results are reproducible
- Test error handling (full filesystem, etc.)

**Reporting Format**:
```
ExFat Filesystem Benchmark Results
==================================
Filesystem: 100 MB, 512B sectors, 32KB clusters

Sequential Write:  45.2 MB/s  (221 ms for 10 MB)
Sequential Read:   67.8 MB/s  (147 ms for 10 MB)

Random Write:      1234 IOPS  (810 µs avg latency)
Random Read:       2456 IOPS  (407 µs avg latency)

Cluster Allocation: 8532 allocs/sec
FAT Updates:        12453 updates/sec

Overall Grade: Excellent
```

**Estimated Effort**: 4-5 hours implementation + 2 hours tests

---

## Implementation Priority

When implementing these in the future, recommended order:

1. **exfatdefrag** (easier, more immediately useful)
   - Pure analysis mode first (read-only)
   - Defragmentation engine later (optional)

2. **exfatbench** (requires more infrastructure)
   - Timing framework
   - Statistical analysis
   - Report generation

## Dependencies

Both utilities depend on:
- ✅ Core ExFat implementation (lib/fs/exfat.inc)
- ✅ Cluster operations (:read_cluster, :write_cluster, :allocate_cluster)
- ✅ FAT operations (:read_fat_entry, :write_fat_entry)
- ⚠️ File operations (currently stubbed - would need implementation)
- ⚠️ Directory traversal (currently not implemented)

**Note**: Full implementation may require completing the file/directory operations in exfat.inc first.

## Contribution Guidelines

When implementing these utilities:
1. Follow ChrysaLisp coding conventions (underscores, :t/:nil, etc.)
2. Include comprehensive test suite (apps/test_<utility>.lisp)
3. Update lib/fs/README.md with usage examples
4. Use consistent error handling (return :nil on error)
5. Provide clear user-facing output
6. Exit with appropriate codes (0 = success, N = errors)

---

**Last Updated**: 2025-11-18
**Status**: Deferred - to be implemented after core utilities are complete
