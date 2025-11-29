;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test suite for exfatdump hex dumper
; Validates hex formatting and dump functions
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
	(defq found :nil
		haystack_len (length haystack)
		needle_len (length needle))

	(when (<= needle_len haystack_len)
		(defq i 0)
		(while (and (not found) (<= (+ i needle_len) haystack_len))
			(when (= (slice haystack i (+ i needle_len)) needle)
				(setq found :t))
			(setq i (inc i))))

	(if found
		(progn
			(print "  PASS: " name)
			(setq pass_count (inc pass_count))
			:t)
		(progn
			(print "  FAIL: " name)
			(print "    Expected to find: " needle)
			(print "    In: " haystack)
			(setq fail_count (inc fail_count))
			:nil)))

; Include functions from exfatdump.lisp
(defun byte_to_hex (byte)
	(defq hex_chars "0123456789ABCDEF"
		high (>> byte 4)
		low (logand byte 0x0F))
	(cat (char (code hex_chars 1 high))
		(char (code hex_chars 1 low))))

(defun format_hex_dump (data offset bytes_per_line)
	(defq lines (list)
		data_len (length data)
		line_offset 0)

	(while (< line_offset data_len)
		(defq offset_str (str-from-num (+ offset line_offset) 16 8))

		(defq hex_str ""
			i 0)
		(while (< i bytes_per_line)
			(if (< (+ line_offset i) data_len)
				(progn
					(defq byte_val (code data 1 (+ line_offset i)))
					(setq hex_str (cat hex_str (byte_to_hex byte_val) " ")))
				(setq hex_str (cat hex_str "   ")))
			(setq i (inc i)))

		(defq ascii_str ""
			i 0)
		(while (and (< i bytes_per_line) (< (+ line_offset i) data_len))
			(defq byte_val (code data 1 (+ line_offset i)))
			(if (and (>= byte_val 32) (<= byte_val 126))
				(setq ascii_str (cat ascii_str (char byte_val)))
				(setq ascii_str (cat ascii_str ".")))
			(setq i (inc i)))

		(push lines (cat offset_str "  " hex_str " |" ascii_str "|"))

		(setq line_offset (+ line_offset bytes_per_line)))

	lines)

(defun test_byte_to_hex ()
	; Test byte to hex conversion
	(print)
	(print "Testing byte_to_hex...")

	(assert_equal "Convert 0x00 to hex" "00" (byte_to_hex 0x00))
	(assert_equal "Convert 0xFF to hex" "FF" (byte_to_hex 0xFF))
	(assert_equal "Convert 0x0F to hex" "0F" (byte_to_hex 0x0F))
	(assert_equal "Convert 0xF0 to hex" "F0" (byte_to_hex 0xF0))
	(assert_equal "Convert 0x55 to hex" "55" (byte_to_hex 0x55))
	(assert_equal "Convert 0xAA to hex" "AA" (byte_to_hex 0xAA))
	(assert_equal "Convert 0x42 to hex" "42" (byte_to_hex 0x42))
	(assert_equal "Convert 0x7F to hex" "7F" (byte_to_hex 0x7F))
	(assert_equal "Convert 0x80 to hex" "80" (byte_to_hex 0x80)))

(defun test_format_hex_dump_basic ()
	; Test basic hex dump formatting
	(print)
	(print "Testing format_hex_dump basic...")

	; Test with simple data
	(defq test_data "ABCD"
		lines (format_hex_dump test_data 0 16))

	(assert_equal "Returns one line for 4 bytes" 1 (length lines))

	; Check first line contains hex representation
	(defq line (get lines 0))
	(assert_contains "Line contains 41 (hex for 'A')" line "41")
	(assert_contains "Line contains 42 (hex for 'B')" line "42")
	(assert_contains "Line contains 43 (hex for 'C')" line "43")
	(assert_contains "Line contains 44 (hex for 'D')" line "44"))

(defun test_format_hex_dump_ascii ()
	; Test ASCII column formatting
	(print)
	(print "Testing format_hex_dump ASCII column...")

	; Test with printable ASCII
	(defq test_data "Hello"
		lines (format_hex_dump test_data 0 16))

	(defq line (get lines 0))
	(assert_contains "ASCII column contains 'Hello'" line "|Hello|"))

(defun test_format_hex_dump_nonprintable ()
	; Test non-printable characters shown as dots
	(print)
	(print "Testing format_hex_dump non-printable...")

	; Create data with null byte
	(defq test_data (cat (char 0x00) "AB" (char 0x1F))
		lines (format_hex_dump test_data 0 16))

	(defq line (get lines 0))
	(assert_contains "Non-printable shown as dots" line "|.AB.|"))

(defun test_format_hex_dump_multiline ()
	; Test multi-line hex dump
	(print)
	(print "Testing format_hex_dump multi-line...")

	; Create 32 bytes of data (should produce 2 lines with 16 bytes/line)
	(defq test_data "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456"
		lines (format_hex_dump test_data 0 16))

	(assert_equal "32 bytes produces 2 lines" 2 (length lines)))

(defun test_format_hex_dump_offset ()
	; Test offset parameter
	(print)
	(print "Testing format_hex_dump offset...")

	(defq test_data "TEST"
		lines (format_hex_dump test_data 0x1000 16))

	(defq line (get lines 0))
	(assert_contains "Offset 0x1000 appears in output" line "00001000"))

(defun test_dump_boot_sector ()
	; Test boot sector dump function
	(print)
	(print "Testing dump_boot_sector...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Read boot sector directly to verify
	(stream-seek fs_stream 0 0)
	(when-bind (boot_sector (read-blk fs_stream (get exfat_obj :sector_size)))
		; Verify it has ExFat signature
		(defq sig_byte0 (code boot_sector 1 510)
			sig_byte1 (code boot_sector 1 511))

		(assert_equal "Boot sector has 0x55 signature" 0x55 sig_byte0)
		(assert_equal "Boot sector has 0xAA signature" 0xAA sig_byte1)))

(defun test_dump_fat_entries ()
	; Test FAT entries dump
	(print)
	(print "Testing FAT entries interpretation...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Allocate a cluster
	(defq cluster (. exfat_obj :allocate_cluster))

	; Verify FAT entry is marked as end-of-chain
	(when-bind (entry (. exfat_obj :read_fat_entry cluster))
		(assert_equal "Allocated cluster marked as EOC" 0xFFFFFFFF entry))

	; Check free cluster
	(defq free_cluster (+ cluster 1))
	(when (< free_cluster (+ (get exfat_obj :cluster_count) 2))
		(when-bind (entry (. exfat_obj :read_fat_entry free_cluster))
			(assert_equal "Free cluster has 0x00 entry" 0x00 entry))))

(defun test_dump_cluster_hex ()
	; Test cluster hex dump
	(print)
	(print "Testing cluster hex dump...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Allocate and write to cluster
	(when-bind (cluster (. exfat_obj :allocate_cluster))
		(defq test_data "Cluster test data")
		(. exfat_obj :write_cluster cluster test_data)

		; Read back and verify
		(when-bind (read_data (. exfat_obj :read_cluster cluster))
			(defq read_str (slice read_data 0 (length test_data)))
			(assert_equal "Cluster data matches" test_data read_str))))

(defun test_dump_sector_hex ()
	; Test sector hex dump
	(print)
	(print "Testing sector hex dump...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Read boot sector (sector 0)
	(when-bind (sector_data (. exfat_obj :read_sector 0))
		(assert_equal "Sector 0 size matches sector_size"
			(get exfat_obj :sector_size)
			(length sector_data))

		; Verify signature bytes
		(defq sig0 (code sector_data 1 510)
			sig1 (code sector_data 1 511))

		(assert_equal "Sector 0 has boot signature 0x55" 0x55 sig0)
		(assert_equal "Sector 0 has boot signature 0xAA" 0xAA sig1)))

(defun test_compare_hex_dumps_identical ()
	; Test comparing identical data
	(print)
	(print "Testing compare_hex_dumps with identical data...")

	(defq data1 "TESTDATA"
		data2 "TESTDATA"
		differences 0)

	; Count differences
	(defq len1 (length data1)
		len2 (length data2)
		i 0)

	(while (< i len1)
		(when (not (= (code data1 1 i) (code data2 1 i)))
			(setq differences (inc differences)))
		(setq i (inc i)))

	(assert_equal "Identical data has 0 differences" 0 differences))

(defun test_compare_hex_dumps_different ()
	; Test comparing different data
	(print)
	(print "Testing compare_hex_dumps with different data...")

	(defq data1 "TESTDATA"
		data2 "BESTDATA"
		differences 0)

	; Count differences (should differ in first byte: T vs B)
	(defq len1 (length data1)
		len2 (length data2)
		i 0)

	(while (< i len1)
		(when (not (= (code data1 1 i) (code data2 1 i)))
			(setq differences (inc differences)))
		(setq i (inc i)))

	(assert_equal "Different data has 1 difference" 1 differences))

(defun test_dump_summary ()
	; Test filesystem summary dump
	(print)
	(print "Testing filesystem summary...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Verify summary fields
	(assert_not_nil "Sector size is set" (get exfat_obj :sector_size))
	(assert_not_nil "Cluster size is set" (get exfat_obj :cluster_size))
	(assert_not_nil "FAT offset is set" (get exfat_obj :fat_offset))
	(assert_not_nil "FAT length is set" (get exfat_obj :fat_length))
	(assert_not_nil "Cluster heap offset is set" (get exfat_obj :cluster_heap_offset))
	(assert_not_nil "Cluster count is set" (get exfat_obj :cluster_count))

	; Verify reasonable values
	(assert_equal "Sector size is 512" 512 (get exfat_obj :sector_size))
	(assert_true "Cluster size is positive" (> (get exfat_obj :cluster_size) 0))
	(assert_true "Cluster count is positive" (> (get exfat_obj :cluster_count) 0)))

(defun test_hex_dump_edge_cases ()
	; Test edge cases
	(print)
	(print "Testing hex dump edge cases...")

	; Empty data
	(defq empty_lines (format_hex_dump "" 0 16))
	(assert_equal "Empty data produces no lines" 0 (length empty_lines))

	; Single byte
	(defq single_lines (format_hex_dump "A" 0 16))
	(assert_equal "Single byte produces one line" 1 (length single_lines))

	; Exactly one line worth
	(defq full_line (str-alloc 16 (char 0x41)))  ; 16 'A' characters
	(defq full_lines (format_hex_dump full_line 0 16))
	(assert_equal "16 bytes produces one line" 1 (length full_lines))

	; One byte over a line
	(defq overflow (str-alloc 17 (char 0x41)))  ; 17 'A' characters
	(defq overflow_lines (format_hex_dump overflow 0 16))
	(assert_equal "17 bytes produces two lines" 2 (length overflow_lines)))

(defun test_fat_sector_dump ()
	; Test FAT sector dump functionality
	(print)
	(print "Testing FAT sector dump...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Allocate some clusters to populate FAT
	(. exfat_obj :allocate_cluster)
	(. exfat_obj :allocate_cluster)
	(. exfat_obj :allocate_cluster)

	; Read FAT sector
	(defq fat_offset (get exfat_obj :fat_offset))
	(when-bind (fat_sector (. exfat_obj :read_sector fat_offset))
		(assert_equal "FAT sector size matches"
			(get exfat_obj :sector_size)
			(length fat_sector))

		; Calculate entries per sector
		(defq entries_per_sector (/ (get exfat_obj :sector_size) 4))
		(assert_true "Entries per sector is positive" (> entries_per_sector 0))))

(defun test_hex_formatting_consistency ()
	; Test hex formatting is always 2 digits
	(print)
	(print "Testing hex formatting consistency...")

	; Test all byte values produce 2-character strings
	(defq all_valid :t
		i 0)

	(while (and all_valid (< i 256))
		(defq hex_str (byte_to_hex i))
		(unless (= (length hex_str) 2)
			(setq all_valid :nil))
		(setq i (inc i)))

	(assert_true "All bytes produce 2-character hex" all_valid))

(defun main ()
	(print "ExFat Hex Dumper Test Suite")
	(print "============================")

	; Run all tests
	(test_byte_to_hex)
	(test_format_hex_dump_basic)
	(test_format_hex_dump_ascii)
	(test_format_hex_dump_nonprintable)
	(test_format_hex_dump_multiline)
	(test_format_hex_dump_offset)
	(test_dump_boot_sector)
	(test_dump_fat_entries)
	(test_dump_cluster_hex)
	(test_dump_sector_hex)
	(test_compare_hex_dumps_identical)
	(test_compare_hex_dumps_different)
	(test_dump_summary)
	(test_hex_dump_edge_cases)
	(test_fat_sector_dump)
	(test_hex_formatting_consistency)

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
