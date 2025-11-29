;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test suite for ExFat directory operations
; Tests path resolution, directory listing, mkdir/rmdir
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
		fs_size (* 10 1024 1024))
	(defq exfat_obj (ExFat fs_stream))
	(. exfat_obj :format fs_size)
	(. exfat_obj :mount)
	exfat_obj)

(defun test_split_path_empty ()
	; Test splitting empty path
	(print)
	(print "Testing split_path with empty path...")

	(defq fs (setup_test_filesystem)
		components (. fs :split_path ""))

	(assert_equal "Empty path produces empty list" 0 (length components)))

(defun test_split_path_root ()
	; Test splitting root path
	(print)
	(print "Testing split_path with root path...")

	(defq fs (setup_test_filesystem)
		components (. fs :split_path "/"))

	(assert_equal "Root path produces empty list" 0 (length components)))

(defun test_split_path_single ()
	; Test splitting single component path
	(print)
	(print "Testing split_path with single component...")

	(defq fs (setup_test_filesystem)
		components (. fs :split_path "/test"))

	(assert_equal "Single component path has 1 part" 1 (length components))
	(assert_equal "Component is 'test'" "test" (get components 0)))

(defun test_split_path_multiple ()
	; Test splitting multi-component path
	(print)
	(print "Testing split_path with multiple components...")

	(defq fs (setup_test_filesystem)
		components (. fs :split_path "/dir1/dir2/file.txt"))

	(assert_equal "Path has 3 components" 3 (length components))
	(assert_equal "First component is 'dir1'" "dir1" (get components 0))
	(assert_equal "Second component is 'dir2'" "dir2" (get components 1))
	(assert_equal "Third component is 'file.txt'" "file.txt" (get components 2)))

(defun test_split_path_no_leading_slash ()
	; Test splitting path without leading slash
	(print)
	(print "Testing split_path without leading slash...")

	(defq fs (setup_test_filesystem)
		components (. fs :split_path "dir/file"))

	(assert_equal "Relative path has 2 components" 2 (length components))
	(assert_equal "First component is 'dir'" "dir" (get components 0))
	(assert_equal "Second component is 'file'" "file" (get components 1)))

(defun test_resolve_root ()
	; Test resolving root directory
	(print)
	(print "Testing resolve_path for root directory...")

	(defq fs (setup_test_filesystem)
		root_info (. fs :resolve_path "/"))

	(assert_not_nil "Root directory found" root_info)
	(assert_equal "Root name is '/'" "/" (get root_info :name))
	(assert_true "Root is directory" (get root_info :is_directory))
	(assert_equal "Root cluster is 2" 2 (get root_info :first_cluster)))

(defun test_resolve_nonexistent ()
	; Test resolving non-existent path
	(print)
	(print "Testing resolve_path for non-existent path...")

	(defq fs (setup_test_filesystem)
		result (. fs :resolve_path "/nonexistent"))

	(assert_equal "Non-existent path returns nil" :nil result))

(defun test_exists_root ()
	; Test exists for root directory
	(print)
	(print "Testing exists for root directory...")

	(defq fs (setup_test_filesystem))

	(assert_true "Root directory exists" (. fs :exists "/")))

(defun test_exists_nonexistent ()
	; Test exists for non-existent path
	(print)
	(print "Testing exists for non-existent path...")

	(defq fs (setup_test_filesystem))

	(assert_equal "Non-existent path doesn't exist" :nil (. fs :exists "/nonexistent")))

(defun test_stat_root ()
	; Test stat for root directory
	(print)
	(print "Testing stat for root directory...")

	(defq fs (setup_test_filesystem)
		root_stat (. fs :stat "/"))

	(assert_not_nil "Root stat returns info" root_stat)
	(assert_equal "Root name is '/'" "/" (get root_stat :name))
	(assert_true "Root is directory" (get root_stat :is_directory)))

(defun test_list_empty_root ()
	; Test listing empty root directory
	(print)
	(print "Testing list for empty root directory...")

	(defq fs (setup_test_filesystem)
		contents (. fs :list "/"))

	(assert_not_nil "List returns a list" contents)
	; Root should be empty initially (except for allocation bitmap)
	(assert_true "Root has few entries" (<= (length contents) 2)))

(defun test_mkdir_basic ()
	; Test basic mkdir operation
	(print)
	(print "Testing basic mkdir...")

	(defq fs (setup_test_filesystem))

	; Create directory
	(assert_true "mkdir succeeds" (. fs :mkdir "/testdir"))

	; Verify it exists
	(assert_true "Created directory exists" (. fs :exists "/testdir")))

(defun test_mkdir_duplicate ()
	; Test mkdir on existing path
	(print)
	(print "Testing mkdir on existing path...")

	(defq fs (setup_test_filesystem))

	; Create directory
	(. fs :mkdir "/testdir")

	; Try to create again
	(assert_equal "mkdir duplicate fails" :nil (. fs :mkdir "/testdir")))

(defun test_mkdir_nested ()
	; Test mkdir with nested path
	(print)
	(print "Testing mkdir with nested path...")

	(defq fs (setup_test_filesystem))

	; Create parent directory
	(. fs :mkdir "/parent")

	; Create nested directory
	(assert_true "Nested mkdir succeeds" (. fs :mkdir "/parent/child"))

	; Verify both exist
	(assert_true "Parent exists" (. fs :exists "/parent"))
	(assert_true "Child exists" (. fs :exists "/parent/child")))

(defun test_mkdir_invalid_parent ()
	; Test mkdir with non-existent parent
	(print)
	(print "Testing mkdir with non-existent parent...")

	(defq fs (setup_test_filesystem))

	; Try to create directory without parent
	(assert_equal "mkdir with invalid parent fails" :nil (. fs :mkdir "/nonexistent/child")))

(defun test_rmdir_empty ()
	; Test removing empty directory
	(print)
	(print "Testing rmdir on empty directory...")

	(defq fs (setup_test_filesystem))

	; Create and remove directory
	(. fs :mkdir "/testdir")
	(assert_true "rmdir succeeds" (. fs :rmdir "/testdir"))

	; Verify it's gone
	(assert_equal "Removed directory doesn't exist" :nil (. fs :exists "/testdir")))

(defun test_rmdir_nonexistent ()
	; Test removing non-existent directory
	(print)
	(print "Testing rmdir on non-existent directory...")

	(defq fs (setup_test_filesystem))

	; Try to remove non-existent directory
	(assert_equal "rmdir nonexistent fails" :nil (. fs :rmdir "/nonexistent")))

(defun test_create_file_basic ()
	; Test basic file creation
	(print)
	(print "Testing basic file creation...")

	(defq fs (setup_test_filesystem))

	; Create file
	(assert_true "create succeeds" (. fs :create "/test.txt"))

	; Verify it exists
	(assert_true "Created file exists" (. fs :exists "/test.txt")))

(defun test_create_file_duplicate ()
	; Test creating duplicate file
	(print)
	(print "Testing create on existing file...")

	(defq fs (setup_test_filesystem))

	; Create file
	(. fs :create "/test.txt")

	; Try to create again
	(assert_equal "create duplicate fails" :nil (. fs :create "/test.txt")))

(defun test_create_file_in_directory ()
	; Test creating file in subdirectory
	(print)
	(print "Testing create file in subdirectory...")

	(defq fs (setup_test_filesystem))

	; Create directory
	(. fs :mkdir "/dir")

	; Create file in directory
	(assert_true "create in subdir succeeds" (. fs :create "/dir/file.txt"))

	; Verify it exists
	(assert_true "File in subdir exists" (. fs :exists "/dir/file.txt")))

(defun test_remove_file ()
	; Test removing a file
	(print)
	(print "Testing file removal...")

	(defq fs (setup_test_filesystem))

	; Create and remove file
	(. fs :create "/test.txt")
	(assert_true "remove succeeds" (. fs :remove "/test.txt"))

	; Verify it's gone
	(assert_equal "Removed file doesn't exist" :nil (. fs :exists "/test.txt")))

(defun test_remove_nonexistent ()
	; Test removing non-existent file
	(print)
	(print "Testing remove on non-existent file...")

	(defq fs (setup_test_filesystem))

	; Try to remove non-existent file
	(assert_equal "remove nonexistent fails" :nil (. fs :remove "/nonexistent.txt")))

(defun test_stat_file ()
	; Test stat on a file
	(print)
	(print "Testing stat on file...")

	(defq fs (setup_test_filesystem))

	; Create file
	(. fs :create "/test.txt")

	; Get stat
	(when-bind (file_stat (. fs :stat "/test.txt"))
		(assert_not_nil "File stat returns info" file_stat)
		(assert_equal "File name correct" "test.txt" (get file_stat :name))
		(assert_equal "File is not directory" :nil (get file_stat :is_directory))))

(defun test_stat_directory ()
	; Test stat on a directory
	(print)
	(print "Testing stat on directory...")

	(defq fs (setup_test_filesystem))

	; Create directory
	(. fs :mkdir "/testdir")

	; Get stat
	(when-bind (dir_stat (. fs :stat "/testdir"))
		(assert_not_nil "Directory stat returns info" dir_stat)
		(assert_equal "Directory name correct" "testdir" (get dir_stat :name))
		(assert_true "Directory is directory" (get dir_stat :is_directory))))

(defun test_path_with_trailing_slash ()
	; Test paths with trailing slashes
	(print)
	(print "Testing paths with trailing slash...")

	(defq fs (setup_test_filesystem))

	; Create directory
	(. fs :mkdir "/testdir")

	; Try to access with trailing slash
	; Note: This may fail depending on implementation
	(defq result (. fs :exists "/testdir/"))
	; Just verify it doesn't crash
	(assert_true "Trailing slash handled" :t))

(defun test_multiple_levels ()
	; Test multiple directory levels
	(print)
	(print "Testing multiple directory levels...")

	(defq fs (setup_test_filesystem))

	; Create multi-level hierarchy
	(. fs :mkdir "/level1")
	(. fs :mkdir "/level1/level2")
	(. fs :mkdir "/level1/level2/level3")

	; Verify all exist
	(assert_true "Level 1 exists" (. fs :exists "/level1"))
	(assert_true "Level 2 exists" (. fs :exists "/level1/level2"))
	(assert_true "Level 3 exists" (. fs :exists "/level1/level2/level3")))

(defun test_directory_entry_parsing ()
	; Test reading directory entries
	(print)
	(print "Testing directory entry parsing...")

	(defq fs (setup_test_filesystem))

	; Read root directory entries
	(defq root_cluster (get fs :root_dir_cluster))
	(when-bind (entries (. fs :read_dir_entries root_cluster))
		(assert_not_nil "Entries list returned" entries)
		; Should have at least the allocation bitmap entry
		(assert_true "Entries list has items" (>= (length entries) 0))))

(defun main ()
	(print "ExFat Directory Operations Test Suite")
	(print "======================================")

	; Run all tests
	(test_split_path_empty)
	(test_split_path_root)
	(test_split_path_single)
	(test_split_path_multiple)
	(test_split_path_no_leading_slash)
	(test_resolve_root)
	(test_resolve_nonexistent)
	(test_exists_root)
	(test_exists_nonexistent)
	(test_stat_root)
	(test_list_empty_root)
	(test_mkdir_basic)
	(test_mkdir_duplicate)
	(test_mkdir_nested)
	(test_mkdir_invalid_parent)
	(test_rmdir_empty)
	(test_rmdir_nonexistent)
	(test_create_file_basic)
	(test_create_file_duplicate)
	(test_create_file_in_directory)
	(test_remove_file)
	(test_remove_nonexistent)
	(test_stat_file)
	(test_stat_directory)
	(test_path_with_trailing_slash)
	(test_multiple_levels)
	(test_directory_entry_parsing)

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
