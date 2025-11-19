;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests for ExFat Defragmentation Analyzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")
(import "apps/exfatdefrag.lisp")

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

(defun test_count_fragments_no_clusters ()
	; Test fragment counting with no clusters
	(print)
	(print "Testing fragment counting with no clusters...")

	(defq fs (setup_test_filesystem))

	; File with no cluster (empty file)
	(defq frag_info (count_fragments fs 0))
	(assert_equal "No clusters - fragment count" 0 (get frag_info 0))
	(assert_equal "No clusters - cluster count" 0 (get frag_info 1)))

(defun test_count_fragments_single_cluster ()
	; Test fragment counting with single cluster
	(print)
	(print "Testing fragment counting with single cluster...")

	(defq fs (setup_test_filesystem))

	; Allocate single cluster
	(defq cluster (. fs :allocate_cluster))
	(. fs :write_fat_entry cluster +exfat_fat_eoc)

	(defq frag_info (count_fragments fs cluster))
	(assert_equal "Single cluster - fragment count" 1 (get frag_info 0))
	(assert_equal "Single cluster - cluster count" 1 (get frag_info 1)))

(defun test_count_fragments_sequential ()
	; Test fragment counting with sequential clusters
	(print)
	(print "Testing fragment counting with sequential clusters...")

	(defq fs (setup_test_filesystem))

	; Allocate sequential clusters: 2 -> 3 -> 4 -> EOC
	(defq c1 (. fs :allocate_cluster)
		c2 (. fs :allocate_cluster)
		c3 (. fs :allocate_cluster))

	(. fs :write_fat_entry c1 c2)
	(. fs :write_fat_entry c2 c3)
	(. fs :write_fat_entry c3 +exfat_fat_eoc)

	(defq frag_info (count_fragments fs c1))
	(assert_equal "Sequential - fragment count" 1 (get frag_info 0))
	(assert_equal "Sequential - cluster count" 3 (get frag_info 1)))

(defun test_count_fragments_non_sequential ()
	; Test fragment counting with non-sequential clusters
	(print)
	(print "Testing fragment counting with non-sequential clusters...")

	(defq fs (setup_test_filesystem))

	; Allocate non-sequential: 2 -> 5 -> 8 -> EOC (3 fragments)
	(defq c1 (. fs :allocate_cluster)
		c2 (. fs :allocate_cluster)
		c3 (. fs :allocate_cluster)
		c4 (. fs :allocate_cluster)
		c5 (. fs :allocate_cluster)
		c6 (. fs :allocate_cluster))

	; Use clusters 2, 5, 8 (skip 3,4,6,7)
	(. fs :write_fat_entry c1 c4)  ; 2 -> 5
	(. fs :write_fat_entry c4 c6)  ; 5 -> 8
	(. fs :write_fat_entry c6 +exfat_fat_eoc)  ; 8 -> EOC

	(defq frag_info (count_fragments fs c1))
	(assert_equal "Non-sequential - fragment count" 3 (get frag_info 0))
	(assert_equal "Non-sequential - cluster count" 3 (get frag_info 1)))

(defun test_count_fragments_mixed ()
	; Test fragment counting with mixed sequential/non-sequential
	(print)
	(print "Testing fragment counting with mixed pattern...")

	(defq fs (setup_test_filesystem))

	; Pattern: 2->3->4 (seq) -> 7->8 (seq) = 2 fragments
	(defq c1 (. fs :allocate_cluster)
		c2 (. fs :allocate_cluster)
		c3 (. fs :allocate_cluster)
		c4 (. fs :allocate_cluster)
		c5 (. fs :allocate_cluster)
		c6 (. fs :allocate_cluster))

	(. fs :write_fat_entry c1 c2)  ; 2 -> 3
	(. fs :write_fat_entry c2 c3)  ; 3 -> 4
	(. fs :write_fat_entry c3 c5)  ; 4 -> 7 (break)
	(. fs :write_fat_entry c5 c6)  ; 7 -> 8
	(. fs :write_fat_entry c6 +exfat_fat_eoc)  ; 8 -> EOC

	(defq frag_info (count_fragments fs c1))
	(assert_equal "Mixed - fragment count" 2 (get frag_info 0))
	(assert_equal "Mixed - cluster count" 5 (get frag_info 1)))

(defun test_analyze_file_empty ()
	; Test analyzing empty file
	(print)
	(print "Testing file analysis for empty file...")

	(defq fs (setup_test_filesystem))
	(. fs :create "/empty.txt")

	(when-bind (file_info (. fs :stat "/empty.txt"))
		(defq frag_info (analyze_file_fragmentation fs file_info))
		(assert_equal "Empty file - fragments" 0 (get frag_info :fragments))
		(assert_equal "Empty file - clusters" 0 (get frag_info :clusters))
		(assert_equal "Empty file - not fragmented" :nil (get frag_info :fragmented))))

(defun test_analyze_file_with_data ()
	; Test analyzing file with data
	(print)
	(print "Testing file analysis for file with data...")

	(defq fs (setup_test_filesystem))
	(. fs :create "/data.txt")

	(when-bind (file_info (. fs :stat "/data.txt"))
		(defq frag_info (analyze_file_fragmentation fs file_info))
		(assert_not_nil "File info exists" frag_info)
		(assert_equal "File name preserved" "data.txt" (get frag_info :name))))

(defun test_analyze_directory ()
	; Test analyzing directory
	(print)
	(print "Testing directory analysis...")

	(defq fs (setup_test_filesystem))
	(. fs :mkdir "/testdir")

	(when-bind (dir_info (. fs :stat "/testdir"))
		(defq frag_info (analyze_file_fragmentation fs dir_info))
		(assert_equal "Directory marked" :t (get frag_info :is_directory))
		(assert_equal "Directory name" "testdir" (get frag_info :name))))

(defun test_collect_all_files_empty ()
	; Test collecting files from empty filesystem
	(print)
	(print "Testing file collection on empty filesystem...")

	(defq fs (setup_test_filesystem))
	(defq all_files (collect_all_files fs "/" (list)))

	(assert_equal "Empty filesystem has no files" 0 (length all_files)))

(defun test_collect_all_files_flat ()
	; Test collecting files from flat structure
	(print)
	(print "Testing file collection from flat structure...")

	(defq fs (setup_test_filesystem))
	(. fs :create "/file1.txt")
	(. fs :create "/file2.txt")
	(. fs :create "/file3.txt")

	(defq all_files (collect_all_files fs "/" (list)))

	(assert_equal "3 files collected" 3 (length all_files)))

(defun test_collect_all_files_nested ()
	; Test collecting files from nested structure
	(print)
	(print "Testing file collection from nested structure...")

	(defq fs (setup_test_filesystem))
	(. fs :mkdir "/dir1")
	(. fs :mkdir "/dir1/subdir")
	(. fs :create "/dir1/file1.txt")
	(. fs :create "/dir1/subdir/file2.txt")

	(defq all_files (collect_all_files fs "/" (list)))

	; Should have: dir1, subdir, file1.txt, file2.txt = 4 items
	(assert_true "At least 4 items collected" (>= (length all_files) 4)))

(defun test_collect_all_files_paths ()
	; Test that full paths are correct
	(print)
	(print "Testing file collection paths...")

	(defq fs (setup_test_filesystem))
	(. fs :mkdir "/test")
	(. fs :create "/test/file.txt")

	(defq all_files (collect_all_files fs "/" (list)))

	; Find the file entry
	(defq file_found :nil)
	(each (lambda (file)
		(when (eql (get file :name) "file.txt")
			(setq file_found :t)
			(assert_equal "File path correct" "/test/file.txt" (get file :path))))
		all_files)

	(assert_true "File was found" file_found))

(defun test_analyze_filesystem_empty ()
	; Test analyzing empty filesystem
	(print)
	(print "Testing filesystem analysis on empty filesystem...")

	(defq fs (setup_test_filesystem))
	(defq stats (analyze_filesystem fs))

	(assert_equal "No files" 0 (get stats :total_files))
	(assert_equal "No directories" 0 (get stats :total_directories))
	(assert_equal "No fragmentation" 0 (get stats :fragmented_files))
	(assert_equal "0% fragmentation" 0 (get stats :fragmentation_percentage)))

(defun test_analyze_filesystem_single_file ()
	; Test analyzing filesystem with single file
	(print)
	(print "Testing filesystem analysis with single file...")

	(defq fs (setup_test_filesystem))
	(. fs :create "/single.txt")

	(defq stats (analyze_filesystem fs))

	(assert_equal "1 file" 1 (get stats :total_files))
	(assert_equal "No fragmented files" 0 (get stats :fragmented_files)))

(defun test_analyze_filesystem_multiple ()
	; Test analyzing filesystem with multiple files
	(print)
	(print "Testing filesystem analysis with multiple files...")

	(defq fs (setup_test_filesystem))
	(. fs :mkdir "/docs")
	(. fs :create "/file1.txt")
	(. fs :create "/docs/file2.txt")

	(defq stats (analyze_filesystem fs))

	(assert_true "At least 2 files" (>= (get stats :total_files) 2))
	(assert_true "At least 1 directory" (>= (get stats :total_directories) 1)))

(defun test_analyze_filesystem_statistics ()
	; Test that statistics are calculated correctly
	(print)
	(print "Testing filesystem statistics calculations...")

	(defq fs (setup_test_filesystem))
	(. fs :create "/f1.txt")
	(. fs :create "/f2.txt")
	(. fs :create "/f3.txt")

	(defq stats (analyze_filesystem fs))

	(assert_not_nil "Stats exist" stats)
	(assert_true "Total fragments >= 0" (>= (get stats :total_fragments) 0))
	(assert_true "Total clusters >= 0" (>= (get stats :total_clusters) 0))
	(assert_true "Fragmentation % valid"
		(and (>= (get stats :fragmentation_percentage) 0)
			(<= (get stats :fragmentation_percentage) 100))))

(defun test_analyze_filesystem_file_details ()
	; Test that file details are included
	(print)
	(print "Testing file details in analysis...")

	(defq fs (setup_test_filesystem))
	(. fs :create "/test.txt")

	(defq stats (analyze_filesystem fs))
	(defq details (get stats :file_details))

	(assert_not_nil "File details exist" details)
	(assert_true "Details is a list" (list? details))
	(assert_equal "One detail entry" 1 (length details)))

(defun test_fragmentation_percentage_calculation ()
	; Test fragmentation percentage edge cases
	(print)
	(print "Testing fragmentation percentage calculation...")

	(defq fs (setup_test_filesystem))

	; Create several files (none should be fragmented in normal allocation)
	(. fs :create "/a.txt")
	(. fs :create "/b.txt")
	(. fs :create "/c.txt")

	(defq stats (analyze_filesystem fs))

	; Percentage should be 0-100 range
	(assert_true "Percentage in range"
		(and (>= (get stats :fragmentation_percentage) 0)
			(<= (get stats :fragmentation_percentage) 100))))

(defun test_average_fragments_calculation ()
	; Test average fragments calculation
	(print)
	(print "Testing average fragments calculation...")

	(defq fs (setup_test_filesystem))
	(. fs :create "/file.txt")

	(defq stats (analyze_filesystem fs))

	; Average should be reasonable (at least 0)
	(assert_true "Average >= 0" (>= (get stats :average_fragments) 0)))

(defun test_print_report_no_crash ()
	; Test that print_fragmentation_report doesn't crash
	(print)
	(print "Testing report printing (no crash)...")

	(defq fs (setup_test_filesystem))
	(. fs :create "/test.txt")

	(defq stats (analyze_filesystem fs))

	; This should not crash
	(print_fragmentation_report stats)
	(assert_true "Report printed successfully" :t))

(defun test_create_test_filesystem_structure ()
	; Test that create_test_filesystem creates proper structure
	(print)
	(print "Testing test filesystem creation...")

	(defq fs (create_test_filesystem 5))

	; Should have created directories
	(assert_true "Projects dir exists" (. fs :exists "/projects"))
	(assert_true "Docs dir exists" (. fs :exists "/docs"))
	(assert_true "Temp dir exists" (. fs :exists "/temp"))

	; Should have created files
	(assert_true "Main file exists" (. fs :exists "/projects/app1/main.lisp"))
	(assert_true "Readme exists" (. fs :exists "/docs/readme.txt")))

(defun main ()
	(print "ExFat Defragmentation Analyzer Tests")
	(print "====================================")

	; Run all tests
	(test_count_fragments_no_clusters)
	(test_count_fragments_single_cluster)
	(test_count_fragments_sequential)
	(test_count_fragments_non_sequential)
	(test_count_fragments_mixed)
	(test_analyze_file_empty)
	(test_analyze_file_with_data)
	(test_analyze_directory)
	(test_collect_all_files_empty)
	(test_collect_all_files_flat)
	(test_collect_all_files_nested)
	(test_collect_all_files_paths)
	(test_analyze_filesystem_empty)
	(test_analyze_filesystem_single_file)
	(test_analyze_filesystem_multiple)
	(test_analyze_filesystem_statistics)
	(test_analyze_filesystem_file_details)
	(test_fragmentation_percentage_calculation)
	(test_average_fragments_calculation)
	(test_print_report_no_crash)
	(test_create_test_filesystem_structure)

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
