;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests for ExFat Performance Benchmark Tool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")
(import "apps/exfatbench.lisp")

(defq test_count 0
	pass_count 0
	fail_count 0)

(defun assert_equal (name expected actual)
	; Assert two values are equal
	(setq test_count (inc test_count))
	(if (= expected actual)
		(progn
			(print "  PASS: " name)
			(setq pass_count (inc pass_count))
			:t)
		(progn
			(print "  FAIL: " name)
			(print "    Expected: " expected)
			(print "    Actual:   " actual)
			(setq fail_count (inc fail_count))
			:nil)))

(defun assert_true (name condition)
	; Assert condition is true
	(assert_equal name :t (if condition :t :nil)))

(defun assert_not_nil (name value)
	; Assert value is not nil
	(setq test_count (inc test_count))
	(if value
		(progn
			(print "  PASS: " name)
			(setq pass_count (inc pass_count))
			:t)
		(progn
			(print "  FAIL: " name " (value is nil)")
			(setq fail_count (inc fail_count))
			:nil)))

(defun setup_test_filesystem ()
	; Create a test filesystem
	(defq fs_stream (memory-stream)
		fs_size (* 10 1024 1024))  ; 10 MB
	(defq exfat_obj (ExFat fs_stream))
	(. exfat_obj :format fs_size)
	(. exfat_obj :mount)
	exfat_obj)

(defun test_get_time_ms ()
	; Test time measurement function
	(print)
	(print "Testing time measurement...")

	(defq time1 (get_time_ms))
	(defq time2 (get_time_ms))

	(assert_true "Time is non-negative" (>= time1 0))
	(assert_true "Time increases" (>= time2 time1)))

(defun test_measure_operation_basic ()
	; Test operation measurement
	(print)
	(print "Testing operation measurement...")

	(defq result (measure_operation
		(lambda () 42)))

	(assert_equal "Returns list" :t (list? result))
	(assert_equal "List has 2 elements" 2 (length result))
	(assert_equal "Result value correct" 42 (get result 1)))

(defun test_measure_operation_timing ()
	; Test that timing is reasonable
	(print)
	(print "Testing operation timing...")

	(defq result (measure_operation
		(lambda ()
			; Do some work
			(defq sum 0)
			(dotimes (i 100)
				(setq sum (+ sum i)))
			sum)))

	(defq duration (get result 0))
	(assert_true "Duration is non-negative" (>= duration 0)))

(defun test_benchmark_cluster_allocation ()
	; Test cluster allocation benchmark
	(print)
	(print "Testing cluster allocation benchmark...")

	(defq fs (setup_test_filesystem))
	(defq result (benchmark_cluster_allocation fs 10))

	(assert_not_nil "Result exists" result)
	(assert_equal "Returns list" :t (list? result))
	(assert_equal "List has 3 elements" 3 (length result))

	(defq duration (get result 0)
		per_sec (get result 1)
		count (get result 2))

	(assert_true "Duration >= 0" (>= duration 0))
	(assert_true "Per second >= 0" (>= per_sec 0))
	(assert_true "Count > 0" (> count 0)))

(defun test_benchmark_cluster_reads ()
	; Test cluster read benchmark
	(print)
	(print "Testing cluster read benchmark...")

	(defq fs (setup_test_filesystem))

	; Allocate some clusters first
	(defq clusters (list))
	(dotimes (i 5)
		(when-bind (cluster (. fs :allocate_cluster))
			(push clusters cluster)))

	(when (> (length clusters) 0)
		(defq result (benchmark_cluster_reads fs clusters 20))

		(assert_not_nil "Result exists" result)
		(assert_equal "List has 3 elements" 3 (length result))

		(defq duration (get result 0)
			per_sec (get result 1))

		(assert_true "Duration >= 0" (>= duration 0))
		(assert_true "Per second >= 0" (>= per_sec 0))))

(defun test_benchmark_cluster_writes ()
	; Test cluster write benchmark
	(print)
	(print "Testing cluster write benchmark...")

	(defq fs (setup_test_filesystem))

	; Allocate some clusters
	(defq clusters (list))
	(dotimes (i 5)
		(when-bind (cluster (. fs :allocate_cluster))
			(push clusters cluster)))

	(when (> (length clusters) 0)
		(defq result (benchmark_cluster_writes fs clusters 20))

		(assert_not_nil "Result exists" result)
		(assert_equal "List has 3 elements" 3 (length result))

		(defq duration (get result 0)
			per_sec (get result 1))

		(assert_true "Duration >= 0" (>= duration 0))
		(assert_true "Per second >= 0" (>= per_sec 0))))

(defun test_benchmark_fat_reads ()
	; Test FAT read benchmark
	(print)
	(print "Testing FAT read benchmark...")

	(defq fs (setup_test_filesystem))

	; Allocate clusters
	(defq clusters (list))
	(dotimes (i 5)
		(when-bind (cluster (. fs :allocate_cluster))
			(push clusters cluster)))

	(when (> (length clusters) 0)
		(defq result (benchmark_fat_reads fs clusters 50))

		(assert_not_nil "Result exists" result)
		(assert_equal "List has 3 elements" 3 (length result))

		(defq duration (get result 0)
			per_sec (get result 1))

		(assert_true "Duration >= 0" (>= duration 0))
		(assert_true "Per second >= 0" (>= per_sec 0))))

(defun test_benchmark_fat_writes ()
	; Test FAT write benchmark
	(print)
	(print "Testing FAT write benchmark...")

	(defq fs (setup_test_filesystem))

	; Allocate clusters
	(defq clusters (list))
	(dotimes (i 5)
		(when-bind (cluster (. fs :allocate_cluster))
			(push clusters cluster)))

	(when (> (length clusters) 0)
		(defq result (benchmark_fat_writes fs clusters 50))

		(assert_not_nil "Result exists" result)
		(assert_equal "List has 3 elements" 3 (length result))

		(defq duration (get result 0)
			per_sec (get result 1))

		(assert_true "Duration >= 0" (>= duration 0))
		(assert_true "Per second >= 0" (>= per_sec 0))))

(defun test_benchmark_file_creation ()
	; Test file creation benchmark
	(print)
	(print "Testing file creation benchmark...")

	(defq fs (setup_test_filesystem))
	(defq result (benchmark_file_creation fs 10))

	(assert_not_nil "Result exists" result)
	(assert_equal "List has 3 elements" 3 (length result))

	(defq duration (get result 0)
		per_sec (get result 1)
		count (get result 2))

	(assert_true "Duration >= 0" (>= duration 0))
	(assert_true "Per second >= 0" (>= per_sec 0))
	(assert_true "Count > 0" (> count 0))

	; Verify files were actually created
	(assert_true "File 0 exists" (. fs :exists "/bench_0.txt"))
	(assert_true "File 1 exists" (. fs :exists "/bench_1.txt")))

(defun test_benchmark_file_deletion ()
	; Test file deletion benchmark
	(print)
	(print "Testing file deletion benchmark...")

	(defq fs (setup_test_filesystem))

	; Create files first
	(defq files (list "/test1.txt" "/test2.txt" "/test3.txt"))
	(each (lambda (f) (. fs :create f)) files)

	(defq result (benchmark_file_deletion fs files))

	(assert_not_nil "Result exists" result)
	(assert_equal "List has 3 elements" 3 (length result))

	(defq duration (get result 0)
		per_sec (get result 1))

	(assert_true "Duration >= 0" (>= duration 0))
	(assert_true "Per second >= 0" (>= per_sec 0))

	; Verify files were deleted
	(assert_equal "File 1 deleted" :nil (. fs :exists "/test1.txt"))
	(assert_equal "File 2 deleted" :nil (. fs :exists "/test2.txt")))

(defun test_benchmark_directory_listing ()
	; Test directory listing benchmark
	(print)
	(print "Testing directory listing benchmark...")

	(defq fs (setup_test_filesystem))

	; Create some files
	(. fs :create "/file1.txt")
	(. fs :create "/file2.txt")

	(defq result (benchmark_directory_listing fs "/" 20))

	(assert_not_nil "Result exists" result)
	(assert_equal "List has 3 elements" 3 (length result))

	(defq duration (get result 0)
		per_sec (get result 1))

	(assert_true "Duration >= 0" (>= duration 0))
	(assert_true "Per second >= 0" (>= per_sec 0)))

(defun test_run_full_benchmark ()
	; Test full benchmark suite
	(print)
	(print "Testing full benchmark suite...")

	(defq fs (setup_test_filesystem))
	(defq results (run_full_benchmark fs))

	(assert_not_nil "Results exist" results)

	; Check that all benchmark categories exist
	(assert_not_nil "Cluster allocation results"
		(get results :cluster_allocation))
	(assert_not_nil "Cluster read results"
		(get results :cluster_reads))
	(assert_not_nil "Cluster write results"
		(get results :cluster_writes))
	(assert_not_nil "FAT read results"
		(get results :fat_reads))
	(assert_not_nil "FAT write results"
		(get results :fat_writes))
	(assert_not_nil "File creation results"
		(get results :file_creation))
	(assert_not_nil "Directory listing results"
		(get results :directory_listing))
	(assert_not_nil "File deletion results"
		(get results :file_deletion)))

(defun test_benchmark_result_structure ()
	; Test that benchmark results have correct structure
	(print)
	(print "Testing benchmark result structure...")

	(defq fs (setup_test_filesystem))
	(defq results (run_full_benchmark fs))

	; Check cluster allocation structure
	(defq alloc_result (get results :cluster_allocation))
	(assert_not_nil "Has duration_ms" (get alloc_result :duration_ms))
	(assert_not_nil "Has per_second" (get alloc_result :per_second))
	(assert_not_nil "Has count" (get alloc_result :count)))

(defun test_calculate_grade_excellent ()
	; Test grade calculation for excellent performance
	(print)
	(print "Testing grade calculation (excellent)...")

	; Create mock results with high scores
	(defq results (hash
		:cluster_allocation (hash :per_second 5000)
		:cluster_reads (hash :per_second 100000)
		:cluster_writes (hash :per_second 50000)))

	(defq grade (calculate_grade results))
	(assert_equal "Grade is Excellent" "Excellent" grade))

(defun test_calculate_grade_poor ()
	; Test grade calculation for poor performance
	(print)
	(print "Testing grade calculation (poor)...")

	; Create mock results with low scores
	(defq results (hash
		:cluster_allocation (hash :per_second 10)
		:cluster_reads (hash :per_second 100)
		:cluster_writes (hash :per_second 100)))

	(defq grade (calculate_grade results))
	(assert_equal "Grade is Poor" "Poor" grade))

(defun test_print_benchmark_report_no_crash ()
	; Test that print_benchmark_report doesn't crash
	(print)
	(print "Testing benchmark report printing...")

	(defq fs (setup_test_filesystem))
	(defq results (run_full_benchmark fs))

	; This should not crash
	(print_benchmark_report results (* 10 1024 1024) 4096)
	(assert_true "Report printed successfully" :t))

(defun test_benchmark_consistency ()
	; Test that benchmarks produce consistent results
	(print)
	(print "Testing benchmark consistency...")

	(defq fs (setup_test_filesystem))

	; Run same benchmark twice
	(defq result1 (benchmark_cluster_allocation fs 5))
	(defq result2 (benchmark_cluster_allocation fs 5))

	(defq count1 (get result1 2)
		count2 (get result2 2))

	; Counts should be the same (same number of allocations)
	(assert_equal "Consistent allocation counts" count1 count2))

(defun test_benchmark_with_empty_list ()
	; Test benchmarks with empty cluster list
	(print)
	(print "Testing benchmarks with empty cluster list...")

	(defq fs (setup_test_filesystem))
	(defq empty_list (list))

	; Should handle empty list gracefully
	(defq result (benchmark_cluster_reads fs empty_list 10))
	(assert_not_nil "Result exists" result))

(defun test_benchmark_zero_operations ()
	; Test benchmarks with zero operations
	(print)
	(print "Testing benchmarks with zero operations...")

	(defq fs (setup_test_filesystem))

	; Allocate some clusters
	(defq clusters (list))
	(dotimes (i 3)
		(when-bind (cluster (. fs :allocate_cluster))
			(push clusters cluster)))

	; Run with 0 operations
	(when (> (length clusters) 0)
		(defq result (benchmark_cluster_reads fs clusters 0))
		(assert_true "Zero ops - duration is 0" (= (get result 0) 0))))

(defun test_timing_accuracy ()
	; Test that timing measurements are reasonable
	(print)
	(print "Testing timing accuracy...")

	; Time a very fast operation
	(defq result (measure_operation (lambda () 1)))
	(defq duration (get result 0))

	; Duration should be non-negative
	(assert_true "Duration is non-negative" (>= duration 0))

	; Duration should be reasonable (less than 1 second = 1000ms)
	(assert_true "Duration is reasonable" (< duration 1000)))

(defun test_large_iteration_count ()
	; Test benchmarks with large iteration counts
	(print)
	(print "Testing large iteration counts...")

	(defq fs (setup_test_filesystem))

	; Allocate clusters
	(defq clusters (list))
	(dotimes (i 5)
		(when-bind (cluster (. fs :allocate_cluster))
			(push clusters cluster)))

	; Run with larger count
	(when (> (length clusters) 0)
		(defq result (benchmark_fat_reads fs clusters 1000))
		(defq actual_count (get result 2))

		(assert_equal "All reads completed" 1000 actual_count)))

(defun test_benchmark_per_second_calculation ()
	; Test that per-second calculations are correct
	(print)
	(print "Testing per-second calculations...")

	(defq fs (setup_test_filesystem))

	; Run a benchmark
	(defq result (benchmark_cluster_allocation fs 10))
	(defq duration (get result 0)
		per_sec (get result 1)
		count (get result 2))

	; If duration > 0, per_sec should be count * 1000 / duration
	(when (> duration 0)
		(defq expected_per_sec (/ (* count 1000) duration))
		(assert_equal "Per second calculation correct" expected_per_sec per_sec)))

(defun main ()
	(print "ExFat Performance Benchmark Tests")
	(print "==================================")

	; Run all tests
	(test_get_time_ms)
	(test_measure_operation_basic)
	(test_measure_operation_timing)
	(test_benchmark_cluster_allocation)
	(test_benchmark_cluster_reads)
	(test_benchmark_cluster_writes)
	(test_benchmark_fat_reads)
	(test_benchmark_fat_writes)
	(test_benchmark_file_creation)
	(test_benchmark_file_deletion)
	(test_benchmark_directory_listing)
	(test_run_full_benchmark)
	(test_benchmark_result_structure)
	(test_calculate_grade_excellent)
	(test_calculate_grade_poor)
	(test_print_benchmark_report_no_crash)
	(test_benchmark_consistency)
	(test_benchmark_with_empty_list)
	(test_benchmark_zero_operations)
	(test_timing_accuracy)
	(test_large_iteration_count)
	(test_benchmark_per_second_calculation)

	; Summary
	(print)
	(print "Test Results")
	(print "============")
	(print "Total tests:  " test_count)
	(print "Passed:       " pass_count)
	(print "Failed:       " fail_count)
	(print)

	(if (= fail_count 0)
		(progn
			(print "All tests PASSED!")
			0)
		(progn
			(print "Some tests FAILED!")
			fail_count)))

; Run the test suite
(main)
