;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Performance Benchmark Tool
; Measures filesystem performance characteristics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

;;;;;;;;;;;;;;;;;;;;
; Timing Helpers
;;;;;;;;;;;;;;;;;;;;

(defun get_time_ms ()
	; Get current time in milliseconds
	; Uses ChrysaLisp time function
	(/ (time) 1000))

(defun measure_operation (operation_fn)
	; Measure execution time of an operation
	; inputs
	; operation_fn = lambda to execute
	; outputs
	; (duration_ms result)
	(defq start_time (get_time_ms))
	(defq result (operation_fn))
	(defq end_time (get_time_ms))
	(defq duration (- end_time start_time))
	(list duration result))

;;;;;;;;;;;;;;;;;;;;
; Benchmark Functions
;;;;;;;;;;;;;;;;;;;;

(defun benchmark_cluster_allocation (exfat_obj count)
	; Benchmark cluster allocation speed
	; inputs
	; exfat_obj = ExFat filesystem object
	; count = number of clusters to allocate
	; outputs
	; (duration_ms allocations_per_sec)
	(defq timing (measure_operation
		(lambda ()
			(defq allocated (list))
			(dotimes (i count)
				(when-bind (cluster (. exfat_obj :allocate_cluster))
					(push allocated cluster)))
			allocated)))

	(defq duration_ms (get timing 0)
		allocated_list (get timing 1)
		actual_count (length allocated_list))

	; Calculate allocations per second
	(defq allocs_per_sec (if (> duration_ms 0)
		(/ (* actual_count 1000) duration_ms)
		0))

	(list duration_ms allocs_per_sec actual_count))

(defun benchmark_cluster_reads (exfat_obj cluster_list count)
	; Benchmark cluster read speed
	; inputs
	; exfat_obj = ExFat filesystem object
	; cluster_list = list of cluster numbers to read
	; count = number of reads to perform
	; outputs
	; (duration_ms reads_per_sec)
	(defq timing (measure_operation
		(lambda ()
			(defq read_count 0)
			(dotimes (i count)
				(defq cluster_idx (% i (length cluster_list))
					cluster (get cluster_list cluster_idx))
				(when (. exfat_obj :read_cluster cluster)
					(setq read_count (inc read_count))))
			read_count)))

	(defq duration_ms (get timing 0)
		actual_reads (get timing 1))

	(defq reads_per_sec (if (> duration_ms 0)
		(/ (* actual_reads 1000) duration_ms)
		0))

	(list duration_ms reads_per_sec actual_reads))

(defun benchmark_cluster_writes (exfat_obj cluster_list count)
	; Benchmark cluster write speed
	; inputs
	; exfat_obj = ExFat filesystem object
	; cluster_list = list of cluster numbers to write
	; count = number of writes to perform
	; outputs
	; (duration_ms writes_per_sec)
	(defq test_data (str-alloc (get exfat_obj :cluster_size)))

	(defq timing (measure_operation
		(lambda ()
			(defq write_count 0)
			(dotimes (i count)
				(defq cluster_idx (% i (length cluster_list))
					cluster (get cluster_list cluster_idx))
				(when (. exfat_obj :write_cluster cluster test_data)
					(setq write_count (inc write_count))))
			write_count)))

	(defq duration_ms (get timing 0)
		actual_writes (get timing 1))

	(defq writes_per_sec (if (> duration_ms 0)
		(/ (* actual_writes 1000) duration_ms)
		0))

	(list duration_ms writes_per_sec actual_writes))

(defun benchmark_fat_reads (exfat_obj cluster_list count)
	; Benchmark FAT entry read speed
	; inputs
	; exfat_obj = ExFat filesystem object
	; cluster_list = list of cluster numbers
	; count = number of FAT reads
	; outputs
	; (duration_ms reads_per_sec)
	(defq timing (measure_operation
		(lambda ()
			(defq read_count 0)
			(dotimes (i count)
				(defq cluster_idx (% i (length cluster_list))
					cluster (get cluster_list cluster_idx))
				(. exfat_obj :read_fat_entry cluster)
				(setq read_count (inc read_count)))
			read_count)))

	(defq duration_ms (get timing 0)
		actual_reads (get timing 1))

	(defq reads_per_sec (if (> duration_ms 0)
		(/ (* actual_reads 1000) duration_ms)
		0))

	(list duration_ms reads_per_sec actual_reads))

(defun benchmark_fat_writes (exfat_obj cluster_list count)
	; Benchmark FAT entry write speed
	; inputs
	; exfat_obj = ExFat filesystem object
	; cluster_list = list of cluster numbers
	; count = number of FAT writes
	; outputs
	; (duration_ms writes_per_sec)
	(defq timing (measure_operation
		(lambda ()
			(defq write_count 0)
			(dotimes (i count)
				(defq cluster_idx (% i (length cluster_list))
					cluster (get cluster_list cluster_idx))
				(. exfat_obj :write_fat_entry cluster +exfat_fat_eoc)
				(setq write_count (inc write_count)))
			write_count)))

	(defq duration_ms (get timing 0)
		actual_writes (get timing 1))

	(defq writes_per_sec (if (> duration_ms 0)
		(/ (* actual_writes 1000) duration_ms)
		0))

	(list duration_ms writes_per_sec actual_writes))

(defun benchmark_file_creation (exfat_obj count)
	; Benchmark file creation speed
	; inputs
	; exfat_obj = ExFat filesystem object
	; count = number of files to create
	; outputs
	; (duration_ms creates_per_sec)
	(defq timing (measure_operation
		(lambda ()
			(defq create_count 0)
			(dotimes (i count)
				(defq filename (cat "/bench_" (str i) ".txt"))
				(when (. exfat_obj :create filename)
					(setq create_count (inc create_count))))
			create_count)))

	(defq duration_ms (get timing 0)
		actual_creates (get timing 1))

	(defq creates_per_sec (if (> duration_ms 0)
		(/ (* actual_creates 1000) duration_ms)
		0))

	(list duration_ms creates_per_sec actual_creates))

(defun benchmark_file_deletion (exfat_obj filenames)
	; Benchmark file deletion speed
	; inputs
	; exfat_obj = ExFat filesystem object
	; filenames = list of filenames to delete
	; outputs
	; (duration_ms deletes_per_sec)
	(defq timing (measure_operation
		(lambda ()
			(defq delete_count 0)
			(each (lambda (filename)
				(when (. exfat_obj :remove filename)
					(setq delete_count (inc delete_count))))
				filenames)
			delete_count)))

	(defq duration_ms (get timing 0)
		actual_deletes (get timing 1))

	(defq deletes_per_sec (if (> duration_ms 0)
		(/ (* actual_deletes 1000) duration_ms)
		0))

	(list duration_ms deletes_per_sec actual_deletes))

(defun benchmark_directory_listing (exfat_obj path count)
	; Benchmark directory listing speed
	; inputs
	; exfat_obj = ExFat filesystem object
	; path = directory path to list
	; count = number of times to list
	; outputs
	; (duration_ms lists_per_sec)
	(defq timing (measure_operation
		(lambda ()
			(defq list_count 0)
			(dotimes (i count)
				(when (. exfat_obj :list path)
					(setq list_count (inc list_count))))
			list_count)))

	(defq duration_ms (get timing 0)
		actual_lists (get timing 1))

	(defq lists_per_sec (if (> duration_ms 0)
		(/ (* actual_lists 1000) duration_ms)
		0))

	(list duration_ms lists_per_sec actual_lists))

(defun benchmark_path_resolution (exfat_obj paths count)
	; Benchmark path resolution speed
	; inputs
	; exfat_obj = ExFat filesystem object
	; paths = list of paths to resolve
	; count = number of resolutions per path
	; outputs
	; (duration_ms resolutions_per_sec)
	(defq timing (measure_operation
		(lambda ()
			(defq resolve_count 0)
			(dotimes (i count)
				(each (lambda (path)
					(. exfat_obj :resolve_path path)
					(setq resolve_count (inc resolve_count)))
					paths))
			resolve_count)))

	(defq duration_ms (get timing 0)
		actual_resolves (get timing 1))

	(defq resolves_per_sec (if (> duration_ms 0)
		(/ (* actual_resolves 1000) duration_ms)
		0))

	(list duration_ms resolves_per_sec actual_resolves))

;;;;;;;;;;;;;;;;;;;;
; Benchmark Suite
;;;;;;;;;;;;;;;;;;;;

(defun run_full_benchmark (exfat_obj)
	; Run complete benchmark suite
	; inputs
	; exfat_obj = ExFat filesystem object
	; outputs
	; hash with all benchmark results
	(print "Running ExFat Performance Benchmarks...")
	(print)

	; Prepare filesystem
	(defq cluster_count 20)

	; Benchmark 1: Cluster Allocation
	(print "  [1/8] Cluster allocation...")
	(defq alloc_result (benchmark_cluster_allocation exfat_obj cluster_count))
	(defq alloc_duration (get alloc_result 0)
		alloc_per_sec (get alloc_result 1)
		alloc_count (get alloc_result 2))

	; Collect allocated clusters for next benchmarks
	(defq allocated_clusters (list))
	(dotimes (i alloc_count)
		(when-bind (cluster (. exfat_obj :allocate_cluster))
			(push allocated_clusters cluster)))

	; Benchmark 2: Cluster Reads
	(print "  [2/8] Cluster reads...")
	(defq read_count 100)
	(defq read_result (if (> (length allocated_clusters) 0)
		(benchmark_cluster_reads exfat_obj allocated_clusters read_count)
		(list 0 0 0)))
	(defq read_duration (get read_result 0)
		reads_per_sec (get read_result 1))

	; Benchmark 3: Cluster Writes
	(print "  [3/8] Cluster writes...")
	(defq write_count 100)
	(defq write_result (if (> (length allocated_clusters) 0)
		(benchmark_cluster_writes exfat_obj allocated_clusters write_count)
		(list 0 0 0)))
	(defq write_duration (get write_result 0)
		writes_per_sec (get write_result 1))

	; Benchmark 4: FAT Reads
	(print "  [4/8] FAT entry reads...")
	(defq fat_read_count 500)
	(defq fat_read_result (if (> (length allocated_clusters) 0)
		(benchmark_fat_reads exfat_obj allocated_clusters fat_read_count)
		(list 0 0 0)))
	(defq fat_read_duration (get fat_read_result 0)
		fat_reads_per_sec (get fat_read_result 1))

	; Benchmark 5: FAT Writes
	(print "  [5/8] FAT entry writes...")
	(defq fat_write_count 500)
	(defq fat_write_result (if (> (length allocated_clusters) 0)
		(benchmark_fat_writes exfat_obj allocated_clusters fat_write_count)
		(list 0 0 0)))
	(defq fat_write_duration (get fat_write_result 0)
		fat_writes_per_sec (get fat_write_result 1))

	; Benchmark 6: File Creation
	(print "  [6/8] File creation...")
	(defq file_count 50)
	(defq create_result (benchmark_file_creation exfat_obj file_count))
	(defq create_duration (get create_result 0)
		creates_per_sec (get create_result 1)
		actual_creates (get create_result 2))

	; Collect created filenames for deletion benchmark
	(defq created_files (list))
	(dotimes (i actual_creates)
		(push created_files (cat "/bench_" (str i) ".txt")))

	; Benchmark 7: Directory Listing
	(print "  [7/8] Directory listing...")
	(defq list_count 100)
	(defq list_result (benchmark_directory_listing exfat_obj "/" list_count))
	(defq list_duration (get list_result 0)
		lists_per_sec (get list_result 1))

	; Benchmark 8: File Deletion
	(print "  [8/8] File deletion...")
	(defq delete_result (benchmark_file_deletion exfat_obj created_files))
	(defq delete_duration (get delete_result 0)
		deletes_per_sec (get delete_result 1))

	(print)

	; Return results hash
	(hash
		:cluster_allocation (hash
			:duration_ms alloc_duration
			:per_second alloc_per_sec
			:count alloc_count)
		:cluster_reads (hash
			:duration_ms read_duration
			:per_second reads_per_sec)
		:cluster_writes (hash
			:duration_ms write_duration
			:per_second writes_per_sec)
		:fat_reads (hash
			:duration_ms fat_read_duration
			:per_second fat_reads_per_sec)
		:fat_writes (hash
			:duration_ms fat_write_duration
			:per_second fat_writes_per_sec)
		:file_creation (hash
			:duration_ms create_duration
			:per_second creates_per_sec
			:count actual_creates)
		:directory_listing (hash
			:duration_ms list_duration
			:per_second lists_per_sec)
		:file_deletion (hash
			:duration_ms delete_duration
			:per_second deletes_per_sec)))

(defun calculate_grade (results)
	; Calculate overall performance grade
	; inputs
	; results = benchmark results hash
	; outputs
	; grade string ("Excellent", "Good", "Fair", "Poor")
	(defq alloc_score (get (get results :cluster_allocation) :per_second)
		read_score (get (get results :cluster_reads) :per_second)
		write_score (get (get results :cluster_writes) :per_second))

	; Simple scoring based on operations per second
	(defq total_score (+ alloc_score (/ read_score 100) (/ write_score 100)))

	(cond
		((> total_score 1000) "Excellent")
		((> total_score 500) "Good")
		((> total_score 100) "Fair")
		(:t "Poor")))

(defun print_benchmark_report (results fs_size cluster_size)
	; Print detailed benchmark report
	; inputs
	; results = benchmark results hash
	; fs_size = filesystem size in bytes
	; cluster_size = cluster size in bytes
	(print "ExFat Filesystem Benchmark Results")
	(print "==================================")
	(print "Filesystem: " (/ fs_size 1048576) " MB, "
		+exfat_sector_size " B sectors, "
		cluster_size " B clusters")
	(print)

	; Cluster operations
	(defq alloc_stats (get results :cluster_allocation))
	(print "Cluster Allocation: " (get alloc_stats :per_second) " allocs/sec  ("
		(get alloc_stats :duration_ms) " ms for " (get alloc_stats :count) " allocs)")

	(defq read_stats (get results :cluster_reads))
	(print "Cluster Reads:      " (get read_stats :per_second) " reads/sec  ("
		(get read_stats :duration_ms) " ms)")

	(defq write_stats (get results :cluster_writes))
	(print "Cluster Writes:     " (get write_stats :per_second) " writes/sec  ("
		(get write_stats :duration_ms) " ms)")
	(print)

	; FAT operations
	(defq fat_read_stats (get results :fat_reads))
	(print "FAT Reads:          " (get fat_read_stats :per_second) " reads/sec  ("
		(get fat_read_stats :duration_ms) " ms)")

	(defq fat_write_stats (get results :fat_writes))
	(print "FAT Writes:         " (get fat_write_stats :per_second) " writes/sec  ("
		(get fat_write_stats :duration_ms) " ms)")
	(print)

	; File operations
	(defq create_stats (get results :file_creation))
	(print "File Creation:      " (get create_stats :per_second) " creates/sec  ("
		(get create_stats :duration_ms) " ms for " (get create_stats :count) " files)")

	(defq delete_stats (get results :file_deletion))
	(print "File Deletion:      " (get delete_stats :per_second) " deletes/sec  ("
		(get delete_stats :duration_ms) " ms)")

	(defq list_stats (get results :directory_listing))
	(print "Directory Listing:  " (get list_stats :per_second) " lists/sec  ("
		(get list_stats :duration_ms) " ms)")
	(print)

	; Overall grade
	(defq grade (calculate_grade results))
	(print "Overall Grade: " grade))

(defun print_usage ()
	; Print usage information
	(print "Usage: exfatbench.lisp <filesystem_size>")
	(print)
	(print "Measures performance of ExFat filesystem operations.")
	(print)
	(print "Arguments:")
	(print "  filesystem_size   Size in MB (e.g., 10 for 10 MB)")
	(print)
	(print "Examples:")
	(print "  exfatbench.lisp 10    # Benchmark 10 MB filesystem")
	(print "  exfatbench.lisp 100   # Benchmark 100 MB filesystem")
	(print)
	(print "The tool creates a filesystem and runs a comprehensive")
	(print "benchmark suite including cluster, FAT, and file operations."))

(defun main ()
	; Main entry point
	(defq args (get-env "args"))

	(when (or (not args) (< (length args) 1))
		(print_usage)
		(return 1))

	; Parse filesystem size
	(defq size_str (get args 0)
		size_mb (num-str size_str))

	(when (or (not size_mb) (<= size_mb 0))
		(print "Error: Invalid filesystem size: " size_str)
		(print_usage)
		(return 1))

	; Create and format filesystem
	(defq fs_size (* size_mb 1024 1024))
	(print "Creating " size_mb " MB filesystem...")
	(defq fs_stream (memory-stream))
	(defq exfat_obj (ExFat fs_stream))
	(. exfat_obj :format fs_size)
	(. exfat_obj :mount)
	(print)

	; Run benchmarks
	(defq results (run_full_benchmark exfat_obj))

	; Print report
	(print_benchmark_report results fs_size (get exfat_obj :cluster_size))
	(print)

	0)

; Run if executed directly
(when (get-env "args")
	(exit (main)))
