;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test suite for exfatprobe filesystem detector
; Validates detection and validation functions
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

; Include functions from exfatprobe.lisp
(defun check_boot_signature (boot_sector)
	(when (>= (length boot_sector) 512)
		(defq sig_byte0 (code boot_sector 1 510)
			sig_byte1 (code boot_sector 1 511))
		(and (= sig_byte0 0x55) (= sig_byte1 0xAA))))

(defun check_filesystem_name (boot_sector)
	(when (>= (length boot_sector) 11)
		(defq fs_name (slice boot_sector 3 11))
		(= fs_name "EXFAT   ")))

(defun check_jump_boot (boot_sector)
	(when (>= (length boot_sector) 3)
		(defq byte0 (code boot_sector 1 0)
			byte1 (code boot_sector 1 1)
			byte2 (code boot_sector 1 2))
		(and (= byte0 0xEB) (= byte1 0x76) (= byte2 0x90))))

(defun read_uint32_le (data offset)
	(when (>= (length data) (+ offset 4))
		(+ (code data 1 offset)
			(<< (code data 1 (+ offset 1)) 8)
			(<< (code data 1 (+ offset 2)) 16)
			(<< (code data 1 (+ offset 3)) 24))))

(defun read_uint64_le (data offset)
	(when (>= (length data) (+ offset 8))
		(+ (code data 1 offset)
			(<< (code data 1 (+ offset 1)) 8)
			(<< (code data 1 (+ offset 2)) 16)
			(<< (code data 1 (+ offset 3)) 24)
			(<< (code data 1 (+ offset 4)) 32)
			(<< (code data 1 (+ offset 5)) 40)
			(<< (code data 1 (+ offset 6)) 48)
			(<< (code data 1 (+ offset 7)) 56))))

(defun extract_filesystem_parameters (boot_sector)
	(when (>= (length boot_sector) 512)
		(defq partition_offset (read_uint64_le boot_sector 64)
			volume_length (read_uint64_le boot_sector 72)
			fat_offset (read_uint32_le boot_sector 80)
			fat_length (read_uint32_le boot_sector 84)
			cluster_heap_offset (read_uint32_le boot_sector 88)
			cluster_count (read_uint32_le boot_sector 92)
			first_cluster_of_root (read_uint32_le boot_sector 96)
			volume_serial (read_uint32_le boot_sector 100)
			file_system_revision (code boot_sector 1 104)
			volume_flags (code boot_sector 1 106)
			bytes_per_sector_shift (code boot_sector 1 108)
			sectors_per_cluster_shift (code boot_sector 1 109))

		(defq bytes_per_sector (<< 1 bytes_per_sector_shift)
			sectors_per_cluster (<< 1 sectors_per_cluster_shift)
			bytes_per_cluster (* bytes_per_sector sectors_per_cluster)
			volume_size_bytes (* volume_length bytes_per_sector)
			fat_size_bytes (* fat_length bytes_per_sector)
			cluster_heap_size_bytes (* cluster_count bytes_per_cluster))

		(list
			:partition_offset partition_offset
			:volume_length volume_length
			:fat_offset fat_offset
			:fat_length fat_length
			:cluster_heap_offset cluster_heap_offset
			:cluster_count cluster_count
			:root_cluster first_cluster_of_root
			:volume_serial volume_serial
			:fs_revision file_system_revision
			:volume_flags volume_flags
			:bytes_per_sector bytes_per_sector
			:sectors_per_cluster sectors_per_cluster
			:bytes_per_cluster bytes_per_cluster
			:volume_size_bytes volume_size_bytes
			:fat_size_bytes fat_size_bytes
			:cluster_heap_size_bytes cluster_heap_size_bytes)))

(defun validate_parameters (params)
	(defq valid :t)
	(defq bps (get params :bytes_per_sector))
	(unless (or (= bps 512) (= bps 1024) (= bps 2048) (= bps 4096))
		(setq valid :nil))
	(defq spc (get params :sectors_per_cluster))
	(unless (and (>= spc 1) (<= spc 256))
		(setq valid :nil))
	(defq cc (get params :cluster_count))
	(unless (> cc 0)
		(setq valid :nil))
	(defq fo (get params :fat_offset))
	(unless (>= fo 24)
		(setq valid :nil))
	(defq cho (get params :cluster_heap_offset)
		fl (get params :fat_length))
	(unless (>= cho (+ fo fl))
		(setq valid :nil))
	(defq rc (get params :root_cluster))
	(unless (>= rc 2)
		(setq valid :nil))
	valid)

(defun probe_exfat_stream (stream)
	(stream-seek stream 0 0)
	(when-bind (boot_sector (read-blk stream 512))
		(when (and (check_boot_signature boot_sector)
				(check_jump_boot boot_sector)
				(check_filesystem_name boot_sector))
			(when-bind (params (extract_filesystem_parameters boot_sector))
				(if (validate_parameters params)
					params
					:nil)))))

(defun test_check_boot_signature_valid ()
	; Test valid boot signature
	(print)
	(print "Testing valid boot signature...")

	; Create boot sector with valid signature
	(defq boot_sector (str-alloc 512 (char 0x00)))
	; Set signature at offset 510-511
	(setq boot_sector (cat
		(slice boot_sector 0 510)
		(char 0x55)
		(char 0xAA)))

	(assert_true "Valid boot signature detected" (check_boot_signature boot_sector)))

(defun test_check_boot_signature_invalid ()
	; Test invalid boot signature
	(print)
	(print "Testing invalid boot signature...")

	; Create boot sector with invalid signature
	(defq boot_sector (str-alloc 512 (char 0x00)))

	(assert_equal "Invalid boot signature rejected" :nil (check_boot_signature boot_sector)))

(defun test_check_filesystem_name_valid ()
	; Test valid ExFat filesystem name
	(print)
	(print "Testing valid filesystem name...")

	; Create boot sector with "EXFAT   " at offset 3
	(defq boot_sector (cat
		(str-alloc 3 (char 0x00))
		"EXFAT   "
		(str-alloc 499 (char 0x00))))

	(assert_true "Valid filesystem name detected" (check_filesystem_name boot_sector)))

(defun test_check_filesystem_name_invalid ()
	; Test invalid filesystem name
	(print)
	(print "Testing invalid filesystem name...")

	; Create boot sector with wrong name
	(defq boot_sector (cat
		(str-alloc 3 (char 0x00))
		"NTFS    "
		(str-alloc 499 (char 0x00))))

	(assert_equal "Invalid filesystem name rejected" :nil (check_filesystem_name boot_sector)))

(defun test_check_jump_boot_valid ()
	; Test valid jump boot instruction
	(print)
	(print "Testing valid jump boot...")

	; Create boot sector with valid jump boot (0xEB 0x76 0x90)
	(defq boot_sector (cat
		(char 0xEB)
		(char 0x76)
		(char 0x90)
		(str-alloc 509 (char 0x00))))

	(assert_true "Valid jump boot detected" (check_jump_boot boot_sector)))

(defun test_check_jump_boot_invalid ()
	; Test invalid jump boot instruction
	(print)
	(print "Testing invalid jump boot...")

	; Create boot sector with invalid jump boot
	(defq boot_sector (str-alloc 512 (char 0x00)))

	(assert_equal "Invalid jump boot rejected" :nil (check_jump_boot boot_sector)))

(defun test_read_uint32_le ()
	; Test reading 32-bit little-endian integers
	(print)
	(print "Testing read_uint32_le...")

	; Create test data: 0x04030201
	(defq data (cat
		(char 0x01)
		(char 0x02)
		(char 0x03)
		(char 0x04)
		(str-alloc 4 (char 0x00))))

	(defq value (read_uint32_le data 0))
	(assert_equal "Read 0x04030201" 0x04030201 value)

	; Test zero value
	(defq zero_data (str-alloc 8 (char 0x00)))
	(assert_equal "Read zero" 0 (read_uint32_le zero_data 0))

	; Test max value
	(defq max_data (cat
		(char 0xFF)
		(char 0xFF)
		(char 0xFF)
		(char 0xFF)
		(str-alloc 4 (char 0x00))))
	(assert_equal "Read 0xFFFFFFFF" 0xFFFFFFFF (read_uint32_le max_data 0)))

(defun test_read_uint64_le ()
	; Test reading 64-bit little-endian integers
	(print)
	(print "Testing read_uint64_le...")

	; Create test data: 0x0807060504030201
	(defq data (cat
		(char 0x01)
		(char 0x02)
		(char 0x03)
		(char 0x04)
		(char 0x05)
		(char 0x06)
		(char 0x07)
		(char 0x08)))

	(defq value (read_uint64_le data 0))
	(assert_equal "Read 0x0807060504030201" 0x0807060504030201 value)

	; Test zero value
	(defq zero_data (str-alloc 8 (char 0x00)))
	(assert_equal "Read zero (64-bit)" 0 (read_uint64_le zero_data 0)))

(defun test_extract_parameters_from_valid_fs ()
	; Test extracting parameters from valid filesystem
	(print)
	(print "Testing parameter extraction from valid filesystem...")

	; Create a real filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Read boot sector
	(stream-seek fs_stream 0 0)
	(when-bind (boot_sector (read-blk fs_stream 512))
		(when-bind (params (extract_filesystem_parameters boot_sector))
			(assert_not_nil "Parameters extracted" params)
			(assert_equal "Bytes per sector is 512" 512 (get params :bytes_per_sector))
			(assert_true "Cluster count is positive" (> (get params :cluster_count) 0))
			(assert_true "FAT offset is valid" (>= (get params :fat_offset) 24))
			(assert_true "Root cluster is valid" (>= (get params :root_cluster) 2)))))

(defun test_validate_parameters_valid ()
	; Test validating valid parameters
	(print)
	(print "Testing parameter validation with valid params...")

	; Create valid parameter set
	(defq params (list
		:bytes_per_sector 512
		:sectors_per_cluster 64
		:cluster_count 1000
		:fat_offset 24
		:fat_length 10
		:cluster_heap_offset 34
		:root_cluster 2))

	(assert_true "Valid parameters accepted" (validate_parameters params)))

(defun test_validate_parameters_invalid_sector_size ()
	; Test rejecting invalid sector size
	(print)
	(print "Testing parameter validation with invalid sector size...")

	; Create params with invalid sector size
	(defq params (list
		:bytes_per_sector 256  ; Invalid!
		:sectors_per_cluster 64
		:cluster_count 1000
		:fat_offset 24
		:fat_length 10
		:cluster_heap_offset 34
		:root_cluster 2))

	(assert_equal "Invalid sector size rejected" :nil (validate_parameters params)))

(defun test_validate_parameters_invalid_root ()
	; Test rejecting invalid root cluster
	(print)
	(print "Testing parameter validation with invalid root cluster...")

	; Create params with invalid root cluster
	(defq params (list
		:bytes_per_sector 512
		:sectors_per_cluster 64
		:cluster_count 1000
		:fat_offset 24
		:fat_length 10
		:cluster_heap_offset 34
		:root_cluster 0))  ; Invalid! Must be >= 2

	(assert_equal "Invalid root cluster rejected" :nil (validate_parameters params)))

(defun test_probe_valid_filesystem ()
	; Test probing valid ExFat filesystem
	(print)
	(print "Testing probe on valid filesystem...")

	; Create valid filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Probe it
	(when-bind (params (probe_exfat_stream fs_stream))
		(assert_not_nil "Probe succeeds on valid filesystem" params)
		(assert_equal "Detected sector size is 512" 512 (get params :bytes_per_sector))
		(assert_true "Detected cluster count is positive" (> (get params :cluster_count) 0))))

(defun test_probe_corrupted_signature ()
	; Test probing filesystem with corrupted signature
	(print)
	(print "Testing probe on corrupted signature...")

	; Create valid filesystem then corrupt signature
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Corrupt boot signature
	(stream-seek fs_stream 510 0)
	(write-blk fs_stream (cat (char 0x00) (char 0x00)))
	(stream-flush fs_stream)

	; Probe should fail
	(assert_equal "Probe fails on corrupted signature" :nil (probe_exfat_stream fs_stream)))

(defun test_probe_empty_stream ()
	; Test probing empty stream
	(print)
	(print "Testing probe on empty stream...")

	; Create empty stream
	(defq empty_stream (memory-stream))

	; Probe should fail
	(assert_equal "Probe fails on empty stream" :nil (probe_exfat_stream empty_stream)))

(defun test_probe_short_data ()
	; Test probing stream with insufficient data
	(print)
	(print "Testing probe on short data...")

	; Create stream with only 100 bytes
	(defq short_stream (memory-stream))
	(write-blk short_stream (str-alloc 100 (char 0x00)))
	(stream-flush short_stream)

	; Probe should fail
	(assert_equal "Probe fails on short data" :nil (probe_exfat_stream short_stream)))

(defun test_multiple_filesystems ()
	; Test probing multiple different filesystems
	(print)
	(print "Testing probe on multiple filesystems...")

	; Create filesystem 1
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_size1 (* 5 1024 1024))

	(. exfat_obj1 :format fs_size1)

	; Create filesystem 2 (different size)
	(defq fs_stream2 (memory-stream)
		exfat_obj2 (ExFat fs_stream2)
		fs_size2 (* 10 1024 1024))

	(. exfat_obj2 :format fs_size2)

	; Probe both
	(when-bind (params1 (probe_exfat_stream fs_stream1))
		(when-bind (params2 (probe_exfat_stream fs_stream2))
			(assert_not_nil "Both probes succeed" :t)

			; Volume sizes should differ
			(defq size1 (get params1 :volume_size_bytes)
				size2 (get params2 :volume_size_bytes))

			(assert_true "Different filesystems have different sizes" (not (= size1 size2))))))

(defun test_parameter_calculations ()
	; Test derived parameter calculations
	(print)
	(print "Testing parameter calculations...")

	; Create filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Probe and verify calculations
	(when-bind (params (probe_exfat_stream fs_stream))
		; bytes_per_cluster = bytes_per_sector * sectors_per_cluster
		(defq expected_bpc (* (get params :bytes_per_sector) (get params :sectors_per_cluster))
			actual_bpc (get params :bytes_per_cluster))

		(assert_equal "Bytes per cluster calculated correctly" expected_bpc actual_bpc)

		; volume_size_bytes = volume_length * bytes_per_sector
		(defq expected_vsb (* (get params :volume_length) (get params :bytes_per_sector))
			actual_vsb (get params :volume_size_bytes))

		(assert_equal "Volume size calculated correctly" expected_vsb actual_vsb)))

(defun main ()
	(print "ExFat Filesystem Probe Test Suite")
	(print "==================================")

	; Run all tests
	(test_check_boot_signature_valid)
	(test_check_boot_signature_invalid)
	(test_check_filesystem_name_valid)
	(test_check_filesystem_name_invalid)
	(test_check_jump_boot_valid)
	(test_check_jump_boot_invalid)
	(test_read_uint32_le)
	(test_read_uint64_le)
	(test_extract_parameters_from_valid_fs)
	(test_validate_parameters_valid)
	(test_validate_parameters_invalid_sector_size)
	(test_validate_parameters_invalid_root)
	(test_probe_valid_filesystem)
	(test_probe_corrupted_signature)
	(test_probe_empty_stream)
	(test_probe_short_data)
	(test_multiple_filesystems)
	(test_parameter_calculations)

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
