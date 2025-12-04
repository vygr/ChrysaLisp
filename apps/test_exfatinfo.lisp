;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test suite for exfatinfo filesystem information dumper
; Validates all information extraction and formatting functions
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

(defun assert_contains (name haystack needle)
	; Assert string contains substring
	(setq test_count (inc test_count))
	(if (find haystack needle)
		(progn
			(print "  PASS: " name)
			(setq pass_count (inc pass_count))
			:t)
		(progn
			(print "  FAIL: " name)
			(print "    String '" haystack "' does not contain '" needle "'")
			(setq fail_count (inc fail_count))
			:nil)))

(defun format_bytes (bytes)
	; Format bytes in human-readable form (from exfatinfo.lisp)
	(cond
		((>= bytes (* 1024 1024 1024))
			(cat (str-from-num (/ bytes (* 1024 1024 1024)) 10) " GB"))
		((>= bytes (* 1024 1024))
			(cat (str-from-num (/ bytes (* 1024 1024)) 10) " MB"))
		((>= bytes 1024)
			(cat (str-from-num (/ bytes 1024) 10) " KB"))
		(:t
			(cat (str-from-num bytes 10) " bytes"))))

(defun test_boot_sector_info_extraction ()
	; Test extraction of boot sector information
	(print)
	(print "Testing boot sector info extraction...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 10 1024 1024))

	(. exfat_obj :format fs_size)

	; Verify all boot sector fields are accessible
	(assert_not_nil "Can get sector size" (get exfat_obj :sector_size))
	(assert_not_nil "Can get cluster size" (get exfat_obj :cluster_size))
	(assert_not_nil "Can get cluster shift" (get exfat_obj :cluster_shift))
	(assert_not_nil "Can get FAT offset" (get exfat_obj :fat_offset))
	(assert_not_nil "Can get FAT length" (get exfat_obj :fat_length))
	(assert_not_nil "Can get cluster heap offset" (get exfat_obj :cluster_heap_offset))
	(assert_not_nil "Can get cluster count" (get exfat_obj :cluster_count))
	(assert_not_nil "Can get root directory cluster" (get exfat_obj :root_dir_cluster))

	; Verify values are reasonable
	(assert_equal "Sector size is 512" 512 (get exfat_obj :sector_size))
	(assert_true "Cluster size is multiple of sector size"
		(= (% (get exfat_obj :cluster_size) (get exfat_obj :sector_size)) 0))
	(assert_equal "Root directory cluster is 2" 2 (get exfat_obj :root_dir_cluster)))

(defun test_geometry_calculations ()
	; Test filesystem geometry calculations
	(print)
	(print "Testing geometry calculations...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 10 1024 1024))

	(. exfat_obj :format fs_size)

	; Calculate sizes
	(defq sector_size (get exfat_obj :sector_size)
		cluster_size (get exfat_obj :cluster_size)
		cluster_count (get exfat_obj :cluster_count)
		fat_length (get exfat_obj :fat_length)
		cluster_heap_offset (get exfat_obj :cluster_heap_offset)
		total_size (. exfat_obj :get_size)
		data_area_size (* cluster_count cluster_size)
		fat_area_size (* fat_length sector_size)
		reserved_area_size (* cluster_heap_offset sector_size))

	(assert_not_nil "Total size is not nil" total_size)
	(assert_true "Data area size is positive" (> data_area_size 0))
	(assert_true "FAT area size is positive" (> fat_area_size 0))
	(assert_true "Reserved area size is positive" (> reserved_area_size 0))

	; Verify relationships
	(assert_true "Total size is sum of components"
		(= total_size (+ data_area_size reserved_area_size)))

	; Test overhead calculation
	(defq overhead_size (- total_size data_area_size)
		overhead_percentage (/ (* overhead_size 100) total_size))

	(print "  Overhead: " overhead_percentage "%")
	(assert_true "Overhead is reasonable (< 10%)" (< overhead_percentage 10)))

(defun test_fat_usage_analysis ()
	; Test FAT usage analysis
	(print)
	(print "Testing FAT usage analysis...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024)
		cluster_count (get exfat_obj :cluster_count)
		+exfat_fat_free 0x00000000
		+exfat_fat_eoc 0xFFFFFFFF)

	(. exfat_obj :format fs_size)

	; Count free clusters
	(defq free_count 0
		allocated_count 0
		bad_count 0)

	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(cond
					((= entry +exfat_fat_free) (setq free_count (inc free_count)))
					((= entry +exfat_fat_eoc) (setq allocated_count (inc allocated_count)))
					((and (>= entry 2) (< entry (+ cluster_count 2)))
						(setq allocated_count (inc allocated_count))))))
		(+ cluster_count 2) :nil 2)

	(print "  Free: " free_count ", Allocated: " allocated_count)

	(assert_true "Free count is positive" (> free_count 0))
	(assert_true "Free + allocated = total"
		(= (+ free_count allocated_count) cluster_count))

	; Allocate some clusters
	(. exfat_obj :allocate_cluster)
	(. exfat_obj :allocate_cluster)

	; Count again
	(defq free_count2 0)
	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(when (= entry +exfat_fat_free)
					(setq free_count2 (inc free_count2)))))
		(+ cluster_count 2) :nil 2)

	(assert_equal "Free count decreased by 2" (- free_count 2) free_count2))

(defun test_byte_formatting ()
	; Test human-readable byte formatting
	(print)
	(print "Testing byte formatting...")

	; Test various sizes
	(assert_contains "Format 512 bytes" (format_bytes 512) "512")
	(assert_contains "Format 1 KB" (format_bytes 1024) "1")
	(assert_contains "Format 1 KB contains KB" (format_bytes 1024) "KB")
	(assert_contains "Format 1 MB" (format_bytes (* 1024 1024)) "1")
	(assert_contains "Format 1 MB contains MB" (format_bytes (* 1024 1024)) "MB")
	(assert_contains "Format 1 GB" (format_bytes (* 1024 1024 1024)) "1")
	(assert_contains "Format 1 GB contains GB" (format_bytes (* 1024 1024 1024)) "GB")

	; Test intermediate values
	(defq result_10kb (format_bytes (* 10 1024)))
	(print "  10 KB formatted as: " result_10kb)
	(assert_contains "Format 10 KB" result_10kb "10"))

(defun test_fat_table_sample ()
	; Test FAT table sample generation
	(print)
	(print "Testing FAT table sample...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Read sample of FAT entries
	(defq sample_size 10
		entries_read 0)

	(each! (lambda (i)
			(when-bind (entry (. exfat_obj :read_fat_entry i))
				(setq entries_read (inc entries_read))))
		sample_size 0)

	(assert_equal "Read all sample entries" sample_size entries_read)

	; Allocate a cluster and verify it appears
	(defq allocated (. exfat_obj :allocate_cluster))
	(when allocated
		(defq entry (. exfat_obj :read_fat_entry allocated))
		(assert_equal "Allocated cluster has EOC marker" 0xFFFFFFFF entry)))

(defun test_cluster_map_generation ()
	; Test cluster allocation map generation
	(print)
	(print "Testing cluster map generation...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024)
		cluster_count (get exfat_obj :cluster_count)
		+exfat_fat_free 0x00000000)

	(. exfat_obj :format fs_size)

	; Generate map symbols for all clusters
	(defq free_symbols 0
		allocated_symbols 0)

	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(if (= entry +exfat_fat_free)
					(setq free_symbols (inc free_symbols))
					(setq allocated_symbols (inc allocated_symbols)))))
		(+ cluster_count 2) :nil 2)

	(print "  Map would show " free_symbols " free + " allocated_symbols " allocated")

	(assert_true "Most clusters are free" (> free_symbols allocated_symbols))
	(assert_equal "All clusters accounted for" cluster_count
		(+ free_symbols allocated_symbols)))

(defun test_fragmentation_analysis ()
	; Test fragmentation analysis
	(print)
	(print "Testing fragmentation analysis...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024)
		cluster_count (get exfat_obj :cluster_count)
		+exfat_fat_eoc 0xFFFFFFFF)

	(. exfat_obj :format fs_size)

	; Count chains
	(defq total_chains 0)
	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(when (= entry +exfat_fat_eoc)
					(setq total_chains (inc total_chains)))))
		(+ cluster_count 2) :nil 2)

	(print "  Found " total_chains " cluster chains")
	(assert_true "At least root chain exists" (>= total_chains 1))

	; Check for sequential allocation
	(defq sequential_count 0
		prev_cluster :nil)

	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(when (and prev_cluster (= entry (+ prev_cluster 1)))
					(setq sequential_count (inc sequential_count)))
				(setq prev_cluster cluster_idx)))
		(+ cluster_count 2) :nil 2)

	(print "  Sequential allocations: " sequential_count))

(defun main ()
	(print "ExFat Information Dumper Test Suite")
	(print "====================================")

	; Run all tests
	(test_boot_sector_info_extraction)
	(test_geometry_calculations)
	(test_fat_usage_analysis)
	(test_byte_formatting)
	(test_fat_table_sample)
	(test_cluster_map_generation)
	(test_fragmentation_analysis)

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
