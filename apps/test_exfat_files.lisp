;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test suite for ExFat file operations
; Tests file open, read, write, seek, close
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

(defun test_create_file_entry ()
	; Test creating a file entry
	(print)
	(print "Testing create_file_entry...")

	(defq fs (setup_test_filesystem)
		entries (. fs :create_file_entry "test.txt" +exfat_attr_archive))

	(assert_not_nil "File entry created" entries)
	(assert_true "Entry list has items" (> (length entries) 0))
	; Should have: file entry + stream entry + name entries
	(assert_true "Entry list has at least 2 items" (>= (length entries) 2)))

(defun test_create_file_entry_long_name ()
	; Test creating entry with long file name
	(print)
	(print "Testing create_file_entry with long name...")

	(defq fs (setup_test_filesystem)
		long_name "verylongfilenamethatexceedsfifteenchars.txt"
		entries (. fs :create_file_entry long_name +exfat_attr_archive))

	(assert_not_nil "Long name entry created" entries)
	; Long name requires more name entries (15 chars per entry)
	(assert_true "Long name has multiple entries" (> (length entries) 2)))

(defun test_parse_file_entry ()
	; Test parsing a file entry
	(print)
	(print "Testing parse_file_entry...")

	(defq fs (setup_test_filesystem))

	; Create entries
	(defq entry_list (. fs :create_file_entry "test.txt" +exfat_attr_archive))

	; Convert to entry hashes (simulate read from disk)
	(defq entries (list))
	(each (lambda (entry_data)
		(defq entry_type (code entry_data 1 0))
		(push entries (hash
			:type entry_type
			:offset 0
			:data entry_data)))
		entry_list)

	; Parse
	(when-bind (file_info (. fs :parse_file_entry entries 0))
		(assert_not_nil "File info parsed" file_info)
		(assert_equal "File name parsed" "test.txt" (get file_info :name))
		(assert_equal "Attributes parsed" +exfat_attr_archive (get file_info :attributes))))

(defun test_file_open_read ()
	; Test opening file for reading
	(print)
	(print "Testing file open in read mode...")

	(defq fs (setup_test_filesystem))

	; Open file (create it first would be in real usage)
	(when-bind (fh (. fs :open "/test.txt" :read))
		(assert_not_nil "File handle created" fh)
		(assert_equal "File path correct" "/test.txt" (get fh :path))
		(assert_equal "File mode is read" :read (get fh :mode))
		(. fs :close fh)))

(defun test_file_open_write ()
	; Test opening file for writing
	(print)
	(print "Testing file open in write mode...")

	(defq fs (setup_test_filesystem))

	; Open file
	(when-bind (fh (. fs :open "/test.txt" :write))
		(assert_not_nil "File handle created for write" fh)
		(assert_equal "File mode is write" :write (get fh :mode))
		(. fs :close fh)))

(defun test_file_close ()
	; Test closing a file
	(print)
	(print "Testing file close...")

	(defq fs (setup_test_filesystem))

	; Open and close file
	(when-bind (fh (. fs :open "/test.txt" :read))
		(assert_true "Close succeeds" (. fs :close fh))

		; Verify file is removed from open files list
		(defq open_files (get fs :open_files))
		(assert_equal "File removed from open list" 0 (length open_files))))

(defun test_file_tell ()
	; Test getting file position
	(print)
	(print "Testing file tell...")

	(defq fs (setup_test_filesystem))

	; Open file
	(when-bind (fh (. fs :open "/test.txt" :read))
		(defq pos (. fs :tell fh))
		(assert_equal "Initial position is 0" 0 pos)
		(. fs :close fh)))

(defun test_file_seek_start ()
	; Test seeking from start
	(print)
	(print "Testing file seek from start...")

	(defq fs (setup_test_filesystem))

	; Open file
	(when-bind (fh (. fs :open "/test.txt" :read))
		(defq new_pos (. fs :seek fh 100 0))  ; whence=0 (start)
		(assert_equal "Seek to 100" 100 new_pos)
		(assert_equal "Tell confirms position" 100 (. fs :tell fh))
		(. fs :close fh)))

(defun test_file_seek_current ()
	; Test seeking from current position
	(print)
	(print "Testing file seek from current...")

	(defq fs (setup_test_filesystem))

	; Open file
	(when-bind (fh (. fs :open "/test.txt" :read))
		; Seek to 50
		(. fs :seek fh 50 0)
		; Seek 25 forward from current
		(defq new_pos (. fs :seek fh 25 1))  ; whence=1 (current)
		(assert_equal "Seek from current to 75" 75 new_pos)
		(. fs :close fh)))

(defun test_file_seek_end ()
	; Test seeking from end
	(print)
	(print "Testing file seek from end...")

	(defq fs (setup_test_filesystem))

	; Open file
	(when-bind (fh (. fs :open "/test.txt" :read))
		; Set file size first
		(def fh :size 1000)
		; Seek -50 from end
		(defq new_pos (. fs :seek fh -50 2))  ; whence=2 (end)
		(assert_equal "Seek from end to 950" 950 new_pos)
		(. fs :close fh)))

(defun test_file_seek_negative ()
	; Test seeking to negative position (should clamp to 0)
	(print)
	(print "Testing file seek to negative position...")

	(defq fs (setup_test_filesystem))

	; Open file
	(when-bind (fh (. fs :open "/test.txt" :read))
		(defq new_pos (. fs :seek fh -100 0))
		(assert_equal "Negative seek clamped to 0" 0 new_pos)
		(. fs :close fh)))

(defun test_file_write_basic ()
	; Test basic file write
	(print)
	(print "Testing basic file write...")

	(defq fs (setup_test_filesystem))

	; Allocate a cluster first
	(when-bind (cluster (. fs :allocate_cluster))
		; Open file with cluster assigned
		(when-bind (fh (. fs :open "/test.txt" :write))
			(def fh :cluster cluster)

			; Write data
			(defq data "Hello, World!")
			(defq bytes_written (. fs :write fh data))

			(assert_equal "Bytes written" (length data) bytes_written)
			(assert_equal "Position updated" (length data) (. fs :tell fh))
			(. fs :close fh))))

(defun test_file_write_empty ()
	; Test writing empty data
	(print)
	(print "Testing write with empty data...")

	(defq fs (setup_test_filesystem))

	; Allocate a cluster
	(when-bind (cluster (. fs :allocate_cluster))
		; Open file
		(when-bind (fh (. fs :open "/test.txt" :write))
			(def fh :cluster cluster)

			; Write empty data
			(defq bytes_written (. fs :write fh ""))

			(assert_equal "Zero bytes written" 0 bytes_written)
			(. fs :close fh))))

(defun test_file_write_no_cluster ()
	; Test writing to file without cluster (should fail)
	(print)
	(print "Testing write without cluster...")

	(defq fs (setup_test_filesystem))

	; Open file without cluster
	(when-bind (fh (. fs :open "/test.txt" :write))
		(def fh :cluster 0)

		; Try to write
		(defq result (. fs :write fh "test"))

		(assert_equal "Write without cluster fails" -1 result)
		(. fs :close fh)))

(defun test_file_read_basic ()
	; Test basic file read
	(print)
	(print "Testing basic file read...")

	(defq fs (setup_test_filesystem))

	; Allocate and write to a cluster
	(when-bind (cluster (. fs :allocate_cluster))
		(defq test_data "Hello, ExFat!")
		(. fs :write_cluster cluster test_data)

		; Open file for reading
		(when-bind (fh (. fs :open "/test.txt" :read))
			(def fh :cluster cluster)

			; Read data
			(when-bind (read_data (. fs :read fh :nil (length test_data)))
				(assert_equal "Read data matches write" test_data read_data)
				(assert_equal "Position updated" (length test_data) (. fs :tell fh)))

			(. fs :close fh))))

(defun test_file_read_partial ()
	; Test partial file read
	(print)
	(print "Testing partial file read...")

	(defq fs (setup_test_filesystem))

	; Allocate and write to a cluster
	(when-bind (cluster (. fs :allocate_cluster))
		(defq test_data "Hello, ExFat!")
		(. fs :write_cluster cluster test_data)

		; Open file for reading
		(when-bind (fh (. fs :open "/test.txt" :read))
			(def fh :cluster cluster)

			; Read only first 5 bytes
			(when-bind (read_data (. fs :read fh :nil 5))
				(assert_equal "Read partial data" "Hello" read_data)
				(assert_equal "Position advanced by 5" 5 (. fs :tell fh)))

			(. fs :close fh))))

(defun test_file_read_beyond_size ()
	; Test reading beyond available data
	(print)
	(print "Testing read beyond data size...")

	(defq fs (setup_test_filesystem))

	; Allocate and write to a cluster
	(when-bind (cluster (. fs :allocate_cluster))
		(defq test_data "Short")
		(. fs :write_cluster cluster test_data)

		; Open file for reading
		(when-bind (fh (. fs :open "/test.txt" :read))
			(def fh :cluster cluster)

			; Try to read more than available
			(when-bind (read_data (. fs :read fh :nil 1000))
				; Should read only what's available
				(assert_true "Read at most available data" (<= (length read_data) (get fs :cluster_size))))

			(. fs :close fh))))

(defun test_file_read_no_cluster ()
	; Test reading from file without cluster
	(print)
	(print "Testing read without cluster...")

	(defq fs (setup_test_filesystem))

	; Open file without cluster
	(when-bind (fh (. fs :open "/test.txt" :read))
		(def fh :cluster 0)

		; Try to read
		(defq result (. fs :read fh :nil 100))

		(assert_equal "Read without cluster returns nil" :nil result)
		(. fs :close fh)))

(defun test_file_read_write_sequence ()
	; Test writing then reading
	(print)
	(print "Testing write then read sequence...")

	(defq fs (setup_test_filesystem))

	; Allocate cluster
	(when-bind (cluster (. fs :allocate_cluster))
		(defq test_data "Test sequence data")

		; Write
		(when-bind (fh_write (. fs :open "/test.txt" :write))
			(def fh_write :cluster cluster)
			(. fs :write fh_write test_data)
			(. fs :close fh_write))

		; Read back
		(when-bind (fh_read (. fs :open "/test.txt" :read))
			(def fh_read :cluster cluster)
			(when-bind (read_data (. fs :read fh_read :nil (length test_data)))
				(assert_equal "Read matches write" test_data read_data))
			(. fs :close fh_read))))

(defun test_multiple_open_files ()
	; Test opening multiple files simultaneously
	(print)
	(print "Testing multiple open files...")

	(defq fs (setup_test_filesystem))

	; Open multiple files
	(when-bind (fh1 (. fs :open "/file1.txt" :read))
		(when-bind (fh2 (. fs :open "/file2.txt" :read))
			(when-bind (fh3 (. fs :open "/file3.txt" :write))
				; Verify all are in open files list
				(defq open_files (get fs :open_files))
				(assert_equal "Three files open" 3 (length open_files))

				; Close all
				(. fs :close fh1)
				(. fs :close fh2)
				(. fs :close fh3)

				; Verify list is empty
				(assert_equal "All files closed" 0 (length (get fs :open_files)))))))

(defun test_rename_validation ()
	; Test rename validation
	(print)
	(print "Testing rename validation...")

	(defq fs (setup_test_filesystem))

	; Create a file
	(. fs :create "/test.txt")

	; Try to rename to existing path (create duplicate)
	(. fs :create "/existing.txt")
	(assert_equal "Rename to existing fails" :nil (. fs :rename "/test.txt" "/existing.txt"))

	; Try to rename nonexistent
	(assert_equal "Rename nonexistent fails" :nil (. fs :rename "/nonexistent.txt" "/new.txt")))

(defun test_file_attributes ()
	; Test file attributes in entry
	(print)
	(print "Testing file attributes...")

	(defq fs (setup_test_filesystem))

	; Create entry with specific attributes
	(defq entries (. fs :create_file_entry "readonly.txt"
		(+ +exfat_attr_read_only +exfat_attr_archive)))

	; Convert to entry hashes
	(defq entry_list (list))
	(each (lambda (entry_data)
		(defq entry_type (code entry_data 1 0))
		(push entry_list (hash :type entry_type :offset 0 :data entry_data)))
		entries)

	; Parse and check
	(when-bind (file_info (. fs :parse_file_entry entry_list 0))
		(defq attrs (get file_info :attributes))
		(assert_true "Read-only attribute set"
			(= (logand attrs +exfat_attr_read_only) +exfat_attr_read_only))
		(assert_true "Archive attribute set"
			(= (logand attrs +exfat_attr_archive) +exfat_attr_archive))))

(defun test_directory_attribute ()
	; Test directory attribute
	(print)
	(print "Testing directory attribute...")

	(defq fs (setup_test_filesystem))

	; Create directory entry
	(defq entries (. fs :create_file_entry "testdir" +exfat_attr_directory))

	; Convert to entry hashes
	(defq entry_list (list))
	(each (lambda (entry_data)
		(defq entry_type (code entry_data 1 0))
		(push entry_list (hash :type entry_type :offset 0 :data entry_data)))
		entries)

	; Parse and check
	(when-bind (dir_info (. fs :parse_file_entry entry_list 0))
		(assert_true "Is directory flag set" (get dir_info :is_directory))))

(defun main ()
	(print "ExFat File Operations Test Suite")
	(print "=================================")

	; Run all tests
	(test_create_file_entry)
	(test_create_file_entry_long_name)
	(test_parse_file_entry)
	(test_file_open_read)
	(test_file_open_write)
	(test_file_close)
	(test_file_tell)
	(test_file_seek_start)
	(test_file_seek_current)
	(test_file_seek_end)
	(test_file_seek_negative)
	(test_file_write_basic)
	(test_file_write_empty)
	(test_file_write_no_cluster)
	(test_file_read_basic)
	(test_file_read_partial)
	(test_file_read_beyond_size)
	(test_file_read_no_cluster)
	(test_file_read_write_sequence)
	(test_multiple_open_files)
	(test_rename_validation)
	(test_file_attributes)
	(test_directory_attribute)

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
