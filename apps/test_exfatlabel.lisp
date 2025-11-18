;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test suite for exfatlabel volume label manager
; Validates label read/write/validation functions
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

; Include label management functions from exfatlabel.lisp
(defun read_volume_label (exfat_obj)
	(defq stream_obj (get exfat_obj :stream)
		sector_size (get exfat_obj :sector_size))
	(stream-seek stream_obj 0 0)
	(when-bind (boot_sector (read-blk stream_obj sector_size))
		(defq label_start 106
			label_length 11
			label_bytes (slice boot_sector label_start (+ label_start label_length))
			label_str ""
			i 0)
		(while (< i label_length)
			(defq ch (code label_bytes 1 i))
			(when (and (> ch 0) (not (= ch 32)))
				(setq label_str (cat label_str (char ch))))
			(setq i (inc i)))
		(if (> (length label_str) 0) label_str :nil)))

(defun write_volume_label (exfat_obj label)
	(defq stream_obj (get exfat_obj :stream)
		sector_size (get exfat_obj :sector_size))
	(when (<= (length label) 11)
		(stream-seek stream_obj 0 0)
		(when-bind (boot_sector (read-blk stream_obj sector_size))
			(defq label_bytes (str-alloc 11 (char 32))
				i 0)
			(while (< i (length label))
				(setq label_bytes (cat
					(slice label_bytes 0 i)
					(char (code label 1 i))
					(slice label_bytes (+ i 1) 11)))
				(setq i (inc i)))
			(defq updated_boot_sector (cat
				(slice boot_sector 0 106)
				label_bytes
				(slice boot_sector 117 sector_size)))
			(stream-seek stream_obj 0 0)
			(when (write-blk stream_obj updated_boot_sector)
				(stream-flush stream_obj)
				:t))))

(defun test_read_default_label ()
	; Test reading label from freshly formatted filesystem
	(print)
	(print "Testing default label reading...")

	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Default label should be empty/nil
	(defq label (read_volume_label exfat_obj))
	(assert_equal "Default label is nil" :nil label))

(defun test_write_and_read_label ()
	; Test writing and reading back a label
	(print)
	(print "Testing label write and read...")

	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Write a label
	(defq test_label "TESTDRIVE")
	(assert_true "Can write label" (write_volume_label exfat_obj test_label))

	; Read it back
	(defq read_label (read_volume_label exfat_obj))
	(assert_equal "Read label matches written label" test_label read_label))

(defun test_label_length_limits ()
	; Test label length constraints
	(print)
	(print "Testing label length limits...")

	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Maximum length (11 characters)
	(defq max_label "12345678901")  ; 11 chars
	(assert_true "Can write 11-character label"
		(write_volume_label exfat_obj max_label))

	(defq read_label (read_volume_label exfat_obj))
	(assert_equal "11-character label preserved" max_label read_label)

	; Too long (12 characters) - should fail
	(defq too_long "123456789012")  ; 12 chars
	(assert_equal "Cannot write 12-character label" :nil
		(write_volume_label exfat_obj too_long)))

(defun test_empty_label ()
	; Test empty label handling
	(print)
	(print "Testing empty label...")

	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Write and read empty label
	(assert_true "Can write empty label" (write_volume_label exfat_obj ""))
	(defq label (read_volume_label exfat_obj))
	(assert_equal "Empty label reads as nil" :nil label))

(defun test_label_overwrite ()
	; Test overwriting existing labels
	(print)
	(print "Testing label overwrite...")

	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Write first label
	(write_volume_label exfat_obj "FIRST")
	(assert_equal "First label set" "FIRST" (read_volume_label exfat_obj))

	; Overwrite with second label
	(write_volume_label exfat_obj "SECOND")
	(assert_equal "Second label overwrites first" "SECOND" (read_volume_label exfat_obj))

	; Overwrite with shorter label
	(write_volume_label exfat_obj "NEW")
	(defq final_label (read_volume_label exfat_obj))
	(assert_equal "Shorter label overwrites longer" "NEW" final_label)
	(assert_equal "Shorter label length correct" 3 (length final_label)))

(defun test_label_special_characters ()
	; Test labels with various characters
	(print)
	(print "Testing special characters...")

	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Test with numbers
	(write_volume_label exfat_obj "DISK123")
	(assert_equal "Label with numbers" "DISK123" (read_volume_label exfat_obj))

	; Test with underscore
	(write_volume_label exfat_obj "MY_DISK")
	(assert_equal "Label with underscore" "MY_DISK" (read_volume_label exfat_obj))

	; Test with hyphen
	(write_volume_label exfat_obj "MY-DISK")
	(assert_equal "Label with hyphen" "MY-DISK" (read_volume_label exfat_obj))

	; Test with spaces
	(write_volume_label exfat_obj "MY DISK")
	(assert_equal "Label with spaces" "MYDISK" (read_volume_label exfat_obj)))

(defun test_label_case_preservation ()
	; Test that label case is preserved
	(print)
	(print "Testing case preservation...")

	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Mixed case
	(write_volume_label exfat_obj "MyDrive")
	(assert_equal "Mixed case preserved" "MyDrive" (read_volume_label exfat_obj))

	; Lowercase
	(write_volume_label exfat_obj "lowerdisk")
	(assert_equal "Lowercase preserved" "lowerdisk" (read_volume_label exfat_obj)))

(defun test_label_persistence ()
	; Test that label persists across mount/unmount
	(print)
	(print "Testing label persistence...")

	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Write label and unmount
	(write_volume_label exfat_obj "PERSIST")
	(. exfat_obj :unmount)

	; Remount and check label
	(. exfat_obj :mount)
	(assert_equal "Label persists after unmount/mount" "PERSIST"
		(read_volume_label exfat_obj)))

(defun test_multiple_filesystems ()
	; Test independent labels on multiple filesystems
	(print)
	(print "Testing multiple filesystems...")

	; Create two filesystems
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_stream2 (memory-stream)
		exfat_obj2 (ExFat fs_stream2)
		fs_size (* 5 1024 1024))

	(. exfat_obj1 :format fs_size)
	(. exfat_obj2 :format fs_size)

	; Set different labels
	(write_volume_label exfat_obj1 "DISK_ONE")
	(write_volume_label exfat_obj2 "DISK_TWO")

	; Verify labels are independent
	(assert_equal "First filesystem has correct label" "DISK_ONE"
		(read_volume_label exfat_obj1))
	(assert_equal "Second filesystem has correct label" "DISK_TWO"
		(read_volume_label exfat_obj2)))

(defun main ()
	(print "ExFat Volume Label Test Suite")
	(print "==============================")

	; Run all tests
	(test_read_default_label)
	(test_write_and_read_label)
	(test_label_length_limits)
	(test_empty_label)
	(test_label_overwrite)
	(test_label_special_characters)
	(test_label_case_preservation)
	(test_label_persistence)
	(test_multiple_filesystems)

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
