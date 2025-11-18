;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test suite for exfatshell interactive shell
; Validates command parsing and execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defq test_count 0
	pass_count 0
	fail_count 0
	test_exfat :nil
	test_current_cluster 2)

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

; Include helper functions from exfatshell.lisp
(defun parse_number (str)
	(when (> (length str) 0)
		(if (and (>= (length str) 2) (= (slice str 0 2) "0x"))
			(str-to-num (slice str 2 (length str)) 16)
			(str-to-num str 10))))

(defun split_command (input)
	(defq parts (list)
		current ""
		i 0)

	(while (< i (length input))
		(defq ch (code input 1 i))
		(if (= ch 32)
			(when (> (length current) 0)
				(push parts current)
				(setq current ""))
			(setq current (cat current (char ch))))
		(setq i (inc i)))

	(when (> (length current) 0)
		(push parts current))

	parts)

(defun setup_test_filesystem ()
	; Create a test filesystem for use in tests
	(defq fs_stream (memory-stream)
		fs_size (* 5 1024 1024))

	(setq test_exfat (ExFat fs_stream))
	(. test_exfat :format fs_size)
	(setq test_current_cluster 2)
	test_exfat)

(defun test_parse_number_decimal ()
	; Test parsing decimal numbers
	(print)
	(print "Testing decimal number parsing...")

	(assert_equal "Parse '42'" 42 (parse_number "42"))
	(assert_equal "Parse '0'" 0 (parse_number "0"))
	(assert_equal "Parse '1000'" 1000 (parse_number "1000"))
	(assert_equal "Parse '255'" 255 (parse_number "255")))

(defun test_parse_number_hex ()
	; Test parsing hexadecimal numbers
	(print)
	(print "Testing hexadecimal number parsing...")

	(assert_equal "Parse '0x10'" 16 (parse_number "0x10"))
	(assert_equal "Parse '0xFF'" 255 (parse_number "0xFF"))
	(assert_equal "Parse '0x00'" 0 (parse_number "0x00"))
	(assert_equal "Parse '0x100'" 256 (parse_number "0x100")))

(defun test_parse_number_invalid ()
	; Test parsing invalid numbers
	(print)
	(print "Testing invalid number parsing...")

	(assert_equal "Empty string returns nil" :nil (parse_number ""))
	; Note: str-to-num may handle invalid formats differently
	)

(defun test_split_command_single ()
	; Test splitting single word command
	(print)
	(print "Testing single word command split...")

	(defq parts (split_command "help"))
	(assert_equal "Single word has 1 part" 1 (length parts))
	(assert_equal "First part is 'help'" "help" (get parts 0)))

(defun test_split_command_multiple ()
	; Test splitting multi-word command
	(print)
	(print "Testing multi-word command split...")

	(defq parts (split_command "cluster 42"))
	(assert_equal "Two words has 2 parts" 2 (length parts))
	(assert_equal "First part is 'cluster'" "cluster" (get parts 0))
	(assert_equal "Second part is '42'" "42" (get parts 1)))

(defun test_split_command_spaces ()
	; Test splitting with multiple spaces
	(print)
	(print "Testing command split with extra spaces...")

	(defq parts (split_command "goto  10"))
	(assert_equal "Extra spaces handled" 2 (length parts))
	(assert_equal "First part correct" "goto" (get parts 0))
	(assert_equal "Second part correct" "10" (get parts 1)))

(defun test_split_command_empty ()
	; Test splitting empty string
	(print)
	(print "Testing empty command split...")

	(defq parts (split_command ""))
	(assert_equal "Empty string produces empty list" 0 (length parts)))

(defun test_filesystem_info ()
	; Test getting filesystem info
	(print)
	(print "Testing filesystem info retrieval...")

	(setup_test_filesystem)

	(assert_not_nil "Filesystem has sector_size" (get test_exfat :sector_size))
	(assert_not_nil "Filesystem has cluster_size" (get test_exfat :cluster_size))
	(assert_not_nil "Filesystem has cluster_count" (get test_exfat :cluster_count))

	(assert_equal "Sector size is 512" 512 (get test_exfat :sector_size))
	(assert_true "Cluster size is positive" (> (get test_exfat :cluster_size) 0))
	(assert_true "Cluster count is positive" (> (get test_exfat :cluster_count) 0)))

(defun test_cluster_allocation ()
	; Test cluster allocation
	(print)
	(print "Testing cluster allocation...")

	(setup_test_filesystem)

	(when-bind (cluster1 (. test_exfat :allocate_cluster))
		(assert_true "First allocation succeeds" (>= cluster1 2))

		(when-bind (entry1 (. test_exfat :read_fat_entry cluster1))
			(assert_equal "Allocated cluster marked EOC" 0xFFFFFFFF entry1))

		(when-bind (cluster2 (. test_exfat :allocate_cluster))
			(assert_true "Second allocation succeeds" (>= cluster2 2))
			(assert_true "Allocations are different" (not (= cluster1 cluster2))))))

(defun test_cluster_read_write ()
	; Test reading and writing clusters
	(print)
	(print "Testing cluster read/write...")

	(setup_test_filesystem)

	(when-bind (cluster (. test_exfat :allocate_cluster))
		(defq test_data "Test data for cluster operations")

		(assert_true "Write succeeds" (. test_exfat :write_cluster cluster test_data))

		(when-bind (read_data (. test_exfat :read_cluster cluster))
			(defq read_str (slice read_data 0 (length test_data)))
			(assert_equal "Read matches write" test_data read_str))))

(defun test_fat_entry_operations ()
	; Test FAT entry operations
	(print)
	(print "Testing FAT entry operations...")

	(setup_test_filesystem)

	; Free cluster should have 0x00 entry
	(when-bind (entry (. test_exfat :read_fat_entry 2))
		(assert_equal "Free cluster has 0x00" 0x00 entry))

	; Allocate cluster
	(when-bind (cluster (. test_exfat :allocate_cluster))
		(when-bind (entry (. test_exfat :read_fat_entry cluster))
			(assert_equal "Allocated cluster has EOC" 0xFFFFFFFF entry))))

(defun test_fat_chain_single ()
	; Test FAT chain with single cluster
	(print)
	(print "Testing single-cluster FAT chain...")

	(setup_test_filesystem)

	(when-bind (cluster (. test_exfat :allocate_cluster))
		; Single cluster chain should be: cluster -> EOC
		(when-bind (entry (. test_exfat :read_fat_entry cluster))
			(assert_equal "Single cluster points to EOC" 0xFFFFFFFF entry))))

(defun test_fat_chain_multiple ()
	; Test FAT chain with multiple clusters
	(print)
	(print "Testing multi-cluster FAT chain...")

	(setup_test_filesystem)

	; Allocate three clusters
	(when-bind (c1 (. test_exfat :allocate_cluster))
		(when-bind (c2 (. test_exfat :allocate_cluster))
			(when-bind (c3 (. test_exfat :allocate_cluster))
				; Link them: c1 -> c2 -> c3 -> EOC
				(. test_exfat :write_fat_entry c1 c2)
				(. test_exfat :write_fat_entry c2 c3)

				; Verify chain
				(when-bind (e1 (. test_exfat :read_fat_entry c1))
					(assert_equal "c1 points to c2" c2 e1))

				(when-bind (e2 (. test_exfat :read_fat_entry c2))
					(assert_equal "c2 points to c3" c3 e2))

				(when-bind (e3 (. test_exfat :read_fat_entry c3))
					(assert_equal "c3 points to EOC" 0xFFFFFFFF e3))))))

(defun test_cluster_free ()
	; Test freeing clusters
	(print)
	(print "Testing cluster free operation...")

	(setup_test_filesystem)

	(when-bind (cluster (. test_exfat :allocate_cluster))
		; Verify allocated
		(when-bind (entry1 (. test_exfat :read_fat_entry cluster))
			(assert_equal "Cluster allocated" 0xFFFFFFFF entry1))

		; Free it
		(assert_true "Free succeeds" (. test_exfat :free_cluster cluster))

		; Verify freed
		(when-bind (entry2 (. test_exfat :read_fat_entry cluster))
			(assert_equal "Cluster freed" 0x00 entry2))))

(defun test_boot_sector_read ()
	; Test reading boot sector
	(print)
	(print "Testing boot sector read...")

	(setup_test_filesystem)

	(defq stream (get test_exfat :stream)
		sector_size (get test_exfat :sector_size))

	(stream-seek stream 0 0)
	(when-bind (boot_sector (read-blk stream sector_size))
		(assert_equal "Boot sector size correct" sector_size (length boot_sector))

		; Check signature
		(defq sig0 (code boot_sector 1 510)
			sig1 (code boot_sector 1 511))

		(assert_equal "Boot signature byte 0 is 0x55" 0x55 sig0)
		(assert_equal "Boot signature byte 1 is 0xAA" 0xAA sig1)

		; Check filesystem name
		(defq fs_name (slice boot_sector 3 11))
		(assert_equal "Filesystem name is EXFAT" "EXFAT   " fs_name)))

(defun test_cluster_statistics ()
	; Test gathering cluster statistics
	(print)
	(print "Testing cluster statistics...")

	(setup_test_filesystem)

	(defq total_clusters (get test_exfat :cluster_count)
		allocated 0
		free 0
		i 2)

	; Allocate a few clusters
	(. test_exfat :allocate_cluster)
	(. test_exfat :allocate_cluster)
	(. test_exfat :allocate_cluster)

	; Count allocations
	(while (< i (+ total_clusters 2))
		(when-bind (entry (. test_exfat :read_fat_entry i))
			(cond
				((= entry 0x00000000) (setq free (inc free)))
				(:t (setq allocated (inc allocated)))))
		(setq i (inc i)))

	(assert_equal "Allocated count is 3" 3 allocated)
	(assert_equal "Free + allocated = total" total_clusters (+ free allocated)))

(defun test_cluster_bounds_checking ()
	; Test cluster bounds checking
	(print)
	(print "Testing cluster bounds checking...")

	(setup_test_filesystem)

	(defq max_cluster (+ (get test_exfat :cluster_count) 1))

	; Try to read beyond bounds
	(assert_equal "Read beyond bounds returns nil" :nil
		(. test_exfat :read_cluster (+ max_cluster 100)))

	; Try to write beyond bounds
	(assert_equal "Write beyond bounds returns nil" :nil
		(. test_exfat :write_cluster (+ max_cluster 100) "test")))

(defun test_cluster_zero_invalid ()
	; Test that cluster 0 and 1 are invalid
	(print)
	(print "Testing invalid cluster numbers...")

	(setup_test_filesystem)

	; Cluster 0 is reserved
	(assert_equal "Cannot read cluster 0" :nil (. test_exfat :read_cluster 0))

	; Cluster 1 is reserved
	(assert_equal "Cannot read cluster 1" :nil (. test_exfat :read_cluster 1)))

(defun test_multiple_allocations ()
	; Test multiple sequential allocations
	(print)
	(print "Testing multiple sequential allocations...")

	(setup_test_filesystem)

	(defq allocations (list))

	; Allocate 10 clusters
	(defq i 0)
	(while (< i 10)
		(when-bind (cluster (. test_exfat :allocate_cluster))
			(push allocations cluster))
		(setq i (inc i)))

	(assert_equal "Allocated 10 clusters" 10 (length allocations))

	; Verify all are unique
	(defq all_unique :t
		i 0)
	(while (and all_unique (< i (length allocations)))
		(defq c1 (get allocations i)
			j (+ i 1))
		(while (and all_unique (< j (length allocations)))
			(when (= c1 (get allocations j))
				(setq all_unique :nil))
			(setq j (inc j)))
		(setq i (inc i)))

	(assert_true "All allocations are unique" all_unique))

(defun test_write_read_large_data ()
	; Test writing and reading larger data
	(print)
	(print "Testing large data write/read...")

	(setup_test_filesystem)

	(when-bind (cluster (. test_exfat :allocate_cluster))
		; Create 1KB of data
		(defq large_data (str-alloc 1024 (char 0x42)))  ; 1KB of 'B'

		(assert_true "Write large data succeeds" (. test_exfat :write_cluster cluster large_data))

		(when-bind (read_data (. test_exfat :read_cluster cluster))
			(defq read_portion (slice read_data 0 1024))

			; Verify first and last bytes
			(assert_equal "First byte correct" 0x42 (code read_portion 1 0))
			(assert_equal "Last byte correct" 0x42 (code read_portion 1 1023)))))

(defun test_fat_entry_interpretation ()
	; Test interpreting different FAT entry values
	(print)
	(print "Testing FAT entry interpretation...")

	(setup_test_filesystem)

	; Test free cluster (0x00000000)
	(when-bind (cluster1 (. test_exfat :allocate_cluster))
		(. test_exfat :free_cluster cluster1)
		(when-bind (entry (. test_exfat :read_fat_entry cluster1))
			(assert_equal "Free cluster is 0x00" 0x00 entry)))

	; Test EOC (0xFFFFFFFF)
	(when-bind (cluster2 (. test_exfat :allocate_cluster))
		(when-bind (entry (. test_exfat :read_fat_entry cluster2))
			(assert_equal "EOC is 0xFFFFFFFF" 0xFFFFFFFF entry)))

	; Test chain link
	(when-bind (cluster3 (. test_exfat :allocate_cluster))
		(when-bind (cluster4 (. test_exfat :allocate_cluster))
			(. test_exfat :write_fat_entry cluster3 cluster4)
			(when-bind (entry (. test_exfat :read_fat_entry cluster3))
				(assert_equal "Chain link points to next cluster" cluster4 entry)))))

(defun test_command_parsing_edge_cases ()
	; Test edge cases in command parsing
	(print)
	(print "Testing command parsing edge cases...")

	; Leading spaces
	(defq parts1 (split_command "  help"))
	(assert_equal "Leading spaces ignored" 1 (length parts1))

	; Trailing spaces
	(defq parts2 (split_command "help  "))
	(assert_equal "Trailing spaces ignored" 1 (length parts2))

	; Multiple arguments
	(defq parts3 (split_command "command arg1 arg2 arg3"))
	(assert_equal "Multiple args parsed" 4 (length parts3))
	(assert_equal "Third arg correct" "arg2" (get parts3 2)))

(defun test_filesystem_get_size ()
	; Test getting filesystem size
	(print)
	(print "Testing filesystem size calculation...")

	(setup_test_filesystem)

	(defq size (. test_exfat :get_size))
	(assert_true "Filesystem size is positive" (> size 0))

	; Should be approximately 5MB
	(assert_true "Size is reasonable" (and (> size 4000000) (< size 6000000))))

(defun main ()
	(print "ExFat Interactive Shell Test Suite")
	(print "===================================")

	; Run all tests
	(test_parse_number_decimal)
	(test_parse_number_hex)
	(test_parse_number_invalid)
	(test_split_command_single)
	(test_split_command_multiple)
	(test_split_command_spaces)
	(test_split_command_empty)
	(test_filesystem_info)
	(test_cluster_allocation)
	(test_cluster_read_write)
	(test_fat_entry_operations)
	(test_fat_chain_single)
	(test_fat_chain_multiple)
	(test_cluster_free)
	(test_boot_sector_read)
	(test_cluster_statistics)
	(test_cluster_bounds_checking)
	(test_cluster_zero_invalid)
	(test_multiple_allocations)
	(test_write_read_large_data)
	(test_fat_entry_interpretation)
	(test_command_parsing_edge_cases)
	(test_filesystem_get_size)

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
