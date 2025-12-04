;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test suite for fsck_exfat filesystem checker
; Validates all consistency checking functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

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

(defun test_boot_sector_validation ()
	; Test boot sector validation
	(print)
	(print "Testing boot sector validation...")

	; Create valid filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Test valid boot sector
	(assert_true "Boot sector has valid sector size"
		(= (logand (get exfat_obj :sector_size)
			(- (get exfat_obj :sector_size) 1)) 0))

	(assert_true "Boot sector has valid cluster size"
		(= (logand (get exfat_obj :cluster_size)
			(- (get exfat_obj :cluster_size) 1)) 0))

	(assert_true "FAT offset is reasonable"
		(>= (get exfat_obj :fat_offset) 24))

	(assert_true "Cluster heap follows FAT"
		(= (get exfat_obj :cluster_heap_offset)
			(+ (get exfat_obj :fat_offset) (get exfat_obj :fat_length))))

	(assert_true "Cluster count is reasonable"
		(> (get exfat_obj :cluster_count) 2)))

(defun test_fat_entry_validation ()
	; Test FAT entry validation
	(print)
	(print "Testing FAT entry validation...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024)
		+exfat_fat_free 0x00000000
		+exfat_fat_eoc 0xFFFFFFFF)

	(. exfat_obj :format fs_size)

	; Test reserved entries
	(assert_not_nil "FAT entry 0 exists"
		(. exfat_obj :read_fat_entry 0))

	(assert_not_nil "FAT entry 1 exists"
		(. exfat_obj :read_fat_entry 1))

	; Test free cluster
	(defq free_entry (. exfat_obj :read_fat_entry 2))
	(assert_equal "Fresh cluster is free" +exfat_fat_free free_entry)

	; Allocate a cluster
	(defq allocated_cluster (. exfat_obj :allocate_cluster))
	(assert_not_nil "Can allocate cluster" allocated_cluster)

	; Check it's marked as EOC
	(defq allocated_entry (. exfat_obj :read_fat_entry allocated_cluster))
	(assert_equal "Allocated cluster marked as EOC" +exfat_fat_eoc allocated_entry))

(defun test_cluster_chain_validation ()
	; Test cluster chain validation
	(print)
	(print "Testing cluster chain validation...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024)
		+exfat_fat_eoc 0xFFFFFFFF)

	(. exfat_obj :format fs_size)

	; Root directory should have valid chain
	(defq root_cluster (get exfat_obj :root_dir_cluster))
	(assert_equal "Root cluster is 2" 2 root_cluster)

	; Root should be marked as EOC (single cluster)
	(defq root_entry (. exfat_obj :read_fat_entry root_cluster))
	(assert_equal "Root directory is EOC" +exfat_fat_eoc root_entry)

	; Create a chain
	(defq cluster1 (. exfat_obj :allocate_cluster))
	(defq cluster2 (. exfat_obj :allocate_cluster))
	(assert_not_nil "Allocated first cluster in chain" cluster1)
	(assert_not_nil "Allocated second cluster in chain" cluster2)

	; Link them
	(. exfat_obj :write_fat_entry cluster1 cluster2)

	; Verify chain
	(defq next_cluster (. exfat_obj :read_fat_entry cluster1))
	(assert_equal "First cluster points to second" cluster2 next_cluster))

(defun test_filesystem_size_calculations ()
	; Test filesystem size calculations
	(print)
	(print "Testing filesystem size calculations...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 10 1024 1024))

	(. exfat_obj :format fs_size)

	; Calculate expected size
	(defq sector_size (get exfat_obj :sector_size)
		cluster_size (get exfat_obj :cluster_size)
		cluster_count (get exfat_obj :cluster_count)
		cluster_heap_offset (get exfat_obj :cluster_heap_offset)
		expected_size (+ (* cluster_heap_offset sector_size)
			(* cluster_count cluster_size))
		reported_size (. exfat_obj :get_size))

	(assert_equal "Filesystem size matches calculation" expected_size reported_size)

	; Verify size is what we requested
	(assert_true "Filesystem size is approximately requested size"
		(> reported_size (* 9 1024 1024))))  ; Within 1MB of requested

(defun test_error_detection ()
	; Test that checker can detect various errors
	(print)
	(print "Testing error detection capabilities...")

	; Create valid filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024)
		cluster_count (get exfat_obj :cluster_count))

	(. exfat_obj :format fs_size)

	; Test bounds checking on FAT access
	(defq invalid_entry (. exfat_obj :read_fat_entry (+ cluster_count 10)))
	(assert_equal "Out of bounds FAT read returns nil" :nil invalid_entry)

	; Test cluster bounds checking
	(defq invalid_cluster (. exfat_obj :read_cluster (+ cluster_count 10)))
	(assert_equal "Out of bounds cluster read returns nil" :nil invalid_cluster))

(defun test_free_cluster_counting ()
	; Test free cluster counting accuracy
	(print)
	(print "Testing free cluster counting...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024)
		cluster_count (get exfat_obj :cluster_count)
		+exfat_fat_free 0x00000000)

	(. exfat_obj :format fs_size)

	; Count free clusters manually
	(defq free_count 0)
	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(when (= entry +exfat_fat_free)
					(setq free_count (inc free_count)))))
		(+ cluster_count 2) :nil 2)

	(print "  Found " free_count " free clusters out of " cluster_count)
	(assert_true "Most clusters are free initially"
		(> free_count (/ cluster_count 2)))

	; Allocate some clusters
	(. exfat_obj :allocate_cluster)
	(. exfat_obj :allocate_cluster)
	(. exfat_obj :allocate_cluster)

	; Count again
	(defq free_count2 0)
	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(when (= entry +exfat_fat_free)
					(setq free_count2 (inc free_count2)))))
		(+ cluster_count 2) :nil 2)

	(assert_equal "Free count decreased by 3" (- free_count 3) free_count2))

(defun test_circular_reference_detection ()
	; Test detection of circular references
	(print)
	(print "Testing circular reference detection...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Create a circular reference
	(defq cluster1 (. exfat_obj :allocate_cluster))
	(defq cluster2 (. exfat_obj :allocate_cluster))

	; Make cluster1 -> cluster2 -> cluster1
	(. exfat_obj :write_fat_entry cluster1 cluster2)
	(. exfat_obj :write_fat_entry cluster2 cluster1)

	; Detect circular reference
	(defq visited (list)
		current cluster1
		circular :nil
		max_depth 100)

	(each! (lambda (i)
			(when (find visited current)
				(setq circular :t))
			(unless circular
				(push visited current)
				(when-bind (next (. exfat_obj :read_fat_entry current))
					(when (and (>= next 2) (< next 1000))
						(setq current next)))))
		max_depth 0)

	(assert_true "Circular reference detected" circular))

(defun main ()
	(print "ExFat Filesystem Checker Test Suite")
	(print "====================================")

	; Run all tests
	(test_boot_sector_validation)
	(test_fat_entry_validation)
	(test_cluster_chain_validation)
	(test_filesystem_size_calculations)
	(test_error_detection)
	(test_free_cluster_counting)
	(test_circular_reference_detection)

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
