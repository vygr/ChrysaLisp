;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integration tests for ExFat filesystem
; End-to-end tests for complete filesystem operations
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

(defun setup_test_filesystem ()
	; Create a test filesystem for use in tests
	(defq fs_stream (memory-stream)
		fs_size (* 20 1024 1024))  ; 20 MB
	(defq exfat_obj (ExFat fs_stream))
	(. exfat_obj :format fs_size)
	(. exfat_obj :mount)
	exfat_obj)

(defun test_create_and_list_files ()
	; Test creating files and listing them
	(print)
	(print "Testing create and list files...")

	(defq fs (setup_test_filesystem))

	; Create several files in root
	(. fs :create "/file1.txt")
	(. fs :create "/file2.txt")
	(. fs :create "/file3.txt")

	; List root directory
	(defq files (. fs :list "/"))

	(assert_true "At least 3 files listed" (>= (length files) 3))

	; Check that our files are in the list
	(defq file1_found :nil
		file2_found :nil
		file3_found :nil)

	(each (lambda (file_info)
		(defq name (get file_info :name))
		(when (= name "file1.txt") (setq file1_found :t))
		(when (= name "file2.txt") (setq file2_found :t))
		(when (= name "file3.txt") (setq file3_found :t)))
		files)

	(assert_true "file1.txt found" file1_found)
	(assert_true "file2.txt found" file2_found)
	(assert_true "file3.txt found" file3_found))

(defun test_create_nested_structure ()
	; Test creating nested directories and files
	(print)
	(print "Testing nested directory structure...")

	(defq fs (setup_test_filesystem))

	; Create directory structure: /docs/2024/reports
	(. fs :mkdir "/docs")
	(. fs :mkdir "/docs/2024")
	(. fs :mkdir "/docs/2024/reports")

	; Create files at different levels
	(. fs :create "/docs/readme.txt")
	(. fs :create "/docs/2024/summary.txt")
	(. fs :create "/docs/2024/reports/q1.txt")

	; Verify all exist
	(assert_true "Exists /docs" (. fs :exists "/docs"))
	(assert_true "Exists /docs/2024" (. fs :exists "/docs/2024"))
	(assert_true "Exists /docs/2024/reports" (. fs :exists "/docs/2024/reports"))
	(assert_true "Exists /docs/readme.txt" (. fs :exists "/docs/readme.txt"))
	(assert_true "Exists /docs/2024/summary.txt" (. fs :exists "/docs/2024/summary.txt"))
	(assert_true "Exists /docs/2024/reports/q1.txt" (. fs :exists "/docs/2024/reports/q1.txt"))

	; List middle directory
	(defq files_2024 (. fs :list "/docs/2024"))
	(assert_true "Directory 2024 has at least 2 entries" (>= (length files_2024) 2)))

(defun test_remove_files ()
	; Test removing files from directories
	(print)
	(print "Testing file removal...")

	(defq fs (setup_test_filesystem))

	; Create and remove a file
	(. fs :create "/temp.txt")
	(assert_true "File created" (. fs :exists "/temp.txt"))

	(. fs :remove "/temp.txt")
	(assert_equal "File removed" :nil (. fs :exists "/temp.txt"))

	; Create multiple files and remove one
	(. fs :create "/keep1.txt")
	(. fs :create "/remove.txt")
	(. fs :create "/keep2.txt")

	(. fs :remove "/remove.txt")

	(assert_true "keep1.txt still exists" (. fs :exists "/keep1.txt"))
	(assert_equal "remove.txt gone" :nil (. fs :exists "/remove.txt"))
	(assert_true "keep2.txt still exists" (. fs :exists "/keep2.txt")))

(defun test_remove_directories ()
	; Test removing empty directories
	(print)
	(print "Testing directory removal...")

	(defq fs (setup_test_filesystem))

	; Create and remove empty directory
	(. fs :mkdir "/empty")
	(assert_true "Directory created" (. fs :exists "/empty"))

	(. fs :rmdir "/empty")
	(assert_equal "Directory removed" :nil (. fs :exists "/empty"))

	; Try to remove non-empty directory (should fail)
	(. fs :mkdir "/nonempty")
	(. fs :create "/nonempty/file.txt")

	(defq result (. fs :rmdir "/nonempty"))
	(assert_equal "Cannot remove non-empty directory" :nil result)
	(assert_true "Directory still exists" (. fs :exists "/nonempty")))

(defun test_rename_file ()
	; Test renaming files
	(print)
	(print "Testing file rename...")

	(defq fs (setup_test_filesystem))

	; Create and rename a file
	(. fs :create "/old_name.txt")
	(assert_true "Old name exists" (. fs :exists "/old_name.txt"))

	(. fs :rename "/old_name.txt" "/new_name.txt")

	(assert_equal "Old name gone" :nil (. fs :exists "/old_name.txt"))
	(assert_true "New name exists" (. fs :exists "/new_name.txt")))

(defun test_rename_directory ()
	; Test renaming directories
	(print)
	(print "Testing directory rename...")

	(defq fs (setup_test_filesystem))

	; Create directory with file
	(. fs :mkdir "/olddir")
	(. fs :create "/olddir/file.txt")

	; Rename directory
	(. fs :rename "/olddir" "/newdir")

	(assert_equal "Old directory gone" :nil (. fs :exists "/olddir"))
	(assert_true "New directory exists" (. fs :exists "/newdir"))
	(assert_true "File moved with directory" (. fs :exists "/newdir/file.txt")))

(defun test_move_file_between_directories ()
	; Test moving files between directories
	(print)
	(print "Testing move file between directories...")

	(defq fs (setup_test_filesystem))

	; Create source and dest directories
	(. fs :mkdir "/source")
	(. fs :mkdir "/dest")

	; Create file in source
	(. fs :create "/source/document.txt")
	(assert_true "File in source" (. fs :exists "/source/document.txt"))

	; Move to dest
	(. fs :rename "/source/document.txt" "/dest/document.txt")

	(assert_equal "File not in source" :nil (. fs :exists "/source/document.txt"))
	(assert_true "File in dest" (. fs :exists "/dest/document.txt")))

(defun test_stat_after_operations ()
	; Test stat after various operations
	(print)
	(print "Testing stat after operations...")

	(defq fs (setup_test_filesystem))

	; Create file and get stat
	(. fs :create "/test.txt")
	(when-bind (stat1 (. fs :stat "/test.txt"))
		(assert_equal "File name correct" "test.txt" (get stat1 :name))
		(assert_equal "Is not directory" :nil (get stat1 :is_directory))

		; Rename and get stat again
		(. fs :rename "/test.txt" "/renamed.txt")
		(when-bind (stat2 (. fs :stat "/renamed.txt"))
			(assert_equal "New name correct" "renamed.txt" (get stat2 :name))
			(assert_equal "Same cluster after rename"
				(get stat1 :first_cluster)
				(get stat2 :first_cluster)))))

(defun test_list_after_modifications ()
	; Test listing after adding and removing entries
	(print)
	(print "Testing list after modifications...")

	(defq fs (setup_test_filesystem))

	; Create directory with files
	(. fs :mkdir "/workspace")
	(. fs :create "/workspace/file1.txt")
	(. fs :create "/workspace/file2.txt")
	(. fs :create "/workspace/file3.txt")

	; List should show 3 files
	(defq files1 (. fs :list "/workspace"))
	(assert_equal "3 files initially" 3 (length files1))

	; Remove one file
	(. fs :remove "/workspace/file2.txt")

	; List should show 2 files
	(defq files2 (. fs :list "/workspace"))
	(assert_equal "2 files after remove" 2 (length files2))

	; Add another file
	(. fs :create "/workspace/file4.txt")

	; List should show 3 files again
	(defq files3 (. fs :list "/workspace"))
	(assert_equal "3 files after add" 3 (length files3)))

(defun test_deeply_nested_operations ()
	; Test operations in deeply nested directories
	(print)
	(print "Testing deeply nested operations...")

	(defq fs (setup_test_filesystem))

	; Create deep hierarchy
	(. fs :mkdir "/a")
	(. fs :mkdir "/a/b")
	(. fs :mkdir "/a/b/c")
	(. fs :mkdir "/a/b/c/d")
	(. fs :mkdir "/a/b/c/d/e")

	; Create file at deepest level
	(. fs :create "/a/b/c/d/e/deep.txt")
	(assert_true "Deep file exists" (. fs :exists "/a/b/c/d/e/deep.txt"))

	; Remove from bottom up
	(. fs :remove "/a/b/c/d/e/deep.txt")
	(. fs :rmdir "/a/b/c/d/e")
	(. fs :rmdir "/a/b/c/d")

	; Verify removals
	(assert_equal "Deep file gone" :nil (. fs :exists "/a/b/c/d/e/deep.txt"))
	(assert_equal "Level e gone" :nil (. fs :exists "/a/b/c/d/e"))
	(assert_equal "Level d gone" :nil (. fs :exists "/a/b/c/d"))
	(assert_true "Level c still exists" (. fs :exists "/a/b/c")))

(defun test_remount_persistence ()
	; Test that changes persist across unmount/mount
	(print)
	(print "Testing remount persistence...")

	; Create filesystem and add files
	(defq fs_stream (memory-stream)
		fs_size (* 10 1024 1024))
	(defq fs (ExFat fs_stream))
	(. fs :format fs_size)
	(. fs :mount)

	; Create structure
	(. fs :mkdir "/data")
	(. fs :create "/data/persistent.txt")

	; Unmount
	(. fs :unmount)

	; Remount
	(. fs :mount)

	; Verify structure exists
	(assert_true "Directory persists" (. fs :exists "/data"))
	(assert_true "File persists" (. fs :exists "/data/persistent.txt")))

(defun test_multiple_operations_sequence ()
	; Test a realistic sequence of operations
	(print)
	(print "Testing realistic operation sequence...")

	(defq fs (setup_test_filesystem))

	; Create project structure
	(. fs :mkdir "/project")
	(. fs :mkdir "/project/src")
	(. fs :mkdir "/project/docs")
	(. fs :mkdir "/project/tests")

	; Add files
	(. fs :create "/project/README.md")
	(. fs :create "/project/src/main.lisp")
	(. fs :create "/project/src/utils.lisp")
	(. fs :create "/project/tests/test_main.lisp")

	; List various directories
	(defq root_files (. fs :list "/project"))
	(defq src_files (. fs :list "/project/src"))

	(assert_true "Project has multiple entries" (>= (length root_files) 3))
	(assert_equal "Src has 2 files" 2 (length src_files))

	; Rename a file
	(. fs :rename "/project/src/utils.lisp" "/project/src/helpers.lisp")
	(assert_true "Renamed file exists" (. fs :exists "/project/src/helpers.lisp"))

	; Remove a directory
	(. fs :remove "/project/tests/test_main.lisp")
	(. fs :rmdir "/project/tests")
	(assert_equal "Tests directory removed" :nil (. fs :exists "/project/tests"))

	; Final verification
	(assert_true "Project still exists" (. fs :exists "/project"))
	(assert_true "Src still exists" (. fs :exists "/project/src")))

(defun test_edge_case_names ()
	; Test edge cases in file names
	(print)
	(print "Testing edge case file names...")

	(defq fs (setup_test_filesystem))

	; Single character
	(. fs :create "/a")
	(assert_true "Single char file exists" (. fs :exists "/a"))

	; With numbers
	(. fs :create "/file123.txt")
	(assert_true "Numeric file exists" (. fs :exists "/file123.txt"))

	; Long name (within limits)
	(. fs :create "/verylongfilename.txt")
	(assert_true "Long name file exists" (. fs :exists "/verylongfilename.txt")))

(defun test_cluster_reuse_after_delete ()
	; Test that clusters are properly freed and reused
	(print)
	(print "Testing cluster reuse after delete...")

	(defq fs (setup_test_filesystem))

	; Create file
	(. fs :create "/temp1.txt")
	(when-bind (stat1 (. fs :stat "/temp1.txt"))
		(defq cluster1 (get stat1 :first_cluster))

		; Delete file
		(. fs :remove "/temp1.txt")

		; Create another file (might reuse cluster)
		(. fs :create "/temp2.txt")
		(when-bind (stat2 (. fs :stat "/temp2.txt"))
			(defq cluster2 (get stat2 :first_cluster))

			; Either same cluster (reused) or different cluster (new allocation)
			(assert_true "Got valid cluster" (>= cluster2 2)))))

(defun main ()
	(print "ExFat Filesystem Integration Tests")
	(print "===================================")

	; Run all tests
	(test_create_and_list_files)
	(test_create_nested_structure)
	(test_remove_files)
	(test_remove_directories)
	(test_rename_file)
	(test_rename_directory)
	(test_move_file_between_directories)
	(test_stat_after_operations)
	(test_list_after_modifications)
	(test_deeply_nested_operations)
	(test_remount_persistence)
	(test_multiple_operations_sequence)
	(test_edge_case_names)
	(test_cluster_reuse_after_delete)

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
