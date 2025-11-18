;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test suite for exfatimage filesystem image tool
; Validates image export/import and cloning functions
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

(defun clone_image (source_exfat dest_stream)
	; Clone filesystem to a new stream (from exfatimage.lisp)
	(defq source_stream (get source_exfat :stream)
		total_size (. source_exfat :get_size)
		chunk_size (* 64 1024)
		bytes_copied 0
		success :t)

	; Seek to beginning of source
	(stream-seek source_stream 0 0)

	; Copy data
	(while (and success (< bytes_copied total_size))
		(defq remaining (- total_size bytes_copied)
			read_size (if (< remaining chunk_size) remaining chunk_size))

		(when-bind (chunk (read-blk source_stream read_size))
			(if (write-blk dest_stream chunk)
				(setq bytes_copied (+ bytes_copied (length chunk)))
				(setq success :nil))))

	(when success
		(stream-flush dest_stream)
		; Create new ExFat object
		(defq dest_exfat (ExFat dest_stream))
		(. dest_exfat :mount)
		dest_exfat))

(defun compare_images (exfat_obj1 exfat_obj2)
	; Compare two filesystem images (from exfatimage.lisp)
	(defq size1 (. exfat_obj1 :get_size)
		size2 (. exfat_obj2 :get_size))

	(if (not (= size1 size2))
		:nil
		(progn
			(defq stream1 (get exfat_obj1 :stream)
				stream2 (get exfat_obj2 :stream)
				chunk_size (* 64 1024)
				bytes_compared 0
				identical :t)

			; Seek to beginning
			(stream-seek stream1 0 0)
			(stream-seek stream2 0 0)

			; Compare chunks
			(while (and identical (< bytes_compared size1))
				(defq remaining (- size1 bytes_compared)
					read_size (if (< remaining chunk_size) remaining chunk_size)
					chunk1 (read-blk stream1 read_size)
					chunk2 (read-blk stream2 read_size))

				(if (and chunk1 chunk2 (= chunk1 chunk2))
					(setq bytes_compared (+ bytes_compared read_size))
					(setq identical :nil)))

			identical)))

(defun test_stream_cloning ()
	; Test basic stream cloning functionality
	(print)
	(print "Testing stream cloning...")

	; Create source filesystem
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_size (* 5 1024 1024))

	(. exfat_obj1 :format fs_size)
	(print "  Created source filesystem")

	; Clone to new stream
	(defq fs_stream2 (memory-stream))
	(when-bind (exfat_obj2 (clone_image exfat_obj1 fs_stream2))
		(print "  Cloned to destination filesystem")

		; Verify sizes match
		(defq size1 (. exfat_obj1 :get_size)
			size2 (. exfat_obj2 :get_size))

		(assert_equal "Clone has same size as original" size1 size2)

		; Verify boot sector fields match
		(assert_equal "Clone has same sector size"
			(get exfat_obj1 :sector_size)
			(get exfat_obj2 :sector_size))

		(assert_equal "Clone has same cluster count"
			(get exfat_obj1 :cluster_count)
			(get exfat_obj2 :cluster_count))))

(defun test_image_comparison ()
	; Test image comparison functionality
	(print)
	(print "Testing image comparison...")

	; Create two identical filesystems
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_size (* 5 1024 1024))

	(. exfat_obj1 :format fs_size)

	; Clone it
	(defq fs_stream2 (memory-stream))
	(when-bind (exfat_obj2 (clone_image exfat_obj1 fs_stream2))

		; Compare identical images
		(assert_true "Identical images compare equal"
			(compare_images exfat_obj1 exfat_obj2))

		; Modify clone
		(. exfat_obj2 :allocate_cluster)

		; Compare modified images
		(assert_true "Modified images are different"
			(not (compare_images exfat_obj1 exfat_obj2)))))

(defun test_clone_preserves_data ()
	; Test that cloning preserves all data
	(print)
	(print "Testing data preservation in clone...")

	; Create filesystem with allocated clusters
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_size (* 5 1024 1024))

	(. exfat_obj1 :format fs_size)

	; Allocate several clusters
	(defq cluster1 (. exfat_obj1 :allocate_cluster))
	(defq cluster2 (. exfat_obj1 :allocate_cluster))
	(defq cluster3 (. exfat_obj1 :allocate_cluster))

	(print "  Allocated " cluster1 ", " cluster2 ", " cluster3)

	; Clone it
	(defq fs_stream2 (memory-stream))
	(when-bind (exfat_obj2 (clone_image exfat_obj1 fs_stream2))

		; Verify allocated clusters are marked in clone
		(defq entry1_orig (. exfat_obj1 :read_fat_entry cluster1))
		(defq entry1_clone (. exfat_obj2 :read_fat_entry cluster1))

		(assert_equal "Cluster 1 FAT entry preserved" entry1_orig entry1_clone)

		(defq entry2_orig (. exfat_obj1 :read_fat_entry cluster2))
		(defq entry2_clone (. exfat_obj2 :read_fat_entry cluster2))

		(assert_equal "Cluster 2 FAT entry preserved" entry2_orig entry2_clone)))

(defun test_clone_with_data_writes ()
	; Test cloning with actual data written to clusters
	(print)
	(print "Testing clone with written data...")

	; Create filesystem
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_size (* 5 1024 1024))

	(. exfat_obj1 :format fs_size)

	; Allocate cluster and write data
	(when-bind (cluster (. exfat_obj1 :allocate_cluster))
		(defq test_data "This is test data for cloning validation")
		(. exfat_obj1 :write_cluster cluster test_data)
		(print "  Wrote test data to cluster " cluster)

		; Clone the filesystem
		(defq fs_stream2 (memory-stream))
		(when-bind (exfat_obj2 (clone_image exfat_obj1 fs_stream2))

			; Read data from clone
			(when-bind (read_data (. exfat_obj2 :read_cluster cluster))
				(defq read_str (slice read_data 0 (length test_data)))
				(assert_equal "Clone contains written data" test_data read_str))))

(defun test_chunked_io_efficiency ()
	; Test that chunked I/O works correctly
	(print)
	(print "Testing chunked I/O...")

	; Create larger filesystem
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_size (* 10 1024 1024))  ; 10 MB

	(. exfat_obj1 :format fs_size)

	(defq total_size (. exfat_obj1 :get_size))
	(print "  Filesystem size: " total_size " bytes")

	; Clone with default chunk size (64KB)
	(defq fs_stream2 (memory-stream)
		chunk_size (* 64 1024)
		expected_chunks (/ (+ total_size chunk_size -1) chunk_size))

	(print "  Expected ~" expected_chunks " chunks")

	(when-bind (exfat_obj2 (clone_image exfat_obj1 fs_stream2))
		(assert_equal "Large filesystem cloned successfully"
			(. exfat_obj1 :get_size)
			(. exfat_obj2 :get_size))))

(defun test_empty_filesystem_clone ()
	; Test cloning empty filesystem
	(print)
	(print "Testing empty filesystem clone...")

	; Create minimal filesystem
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_size (* 1 1024 1024))  ; 1 MB minimum

	(. exfat_obj1 :format fs_size)

	; Clone without any modifications
	(defq fs_stream2 (memory-stream))
	(when-bind (exfat_obj2 (clone_image exfat_obj1 fs_stream2))

		; Verify sizes match
		(assert_equal "Empty filesystem sizes match"
			(. exfat_obj1 :get_size)
			(. exfat_obj2 :get_size))

		; Verify they compare as identical
		(assert_true "Empty filesystems are identical"
			(compare_images exfat_obj1 exfat_obj2))))

(defun test_multiple_clones ()
	; Test creating multiple clones
	(print)
	(print "Testing multiple sequential clones...")

	; Create source
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_size (* 5 1024 1024))

	(. exfat_obj1 :format fs_size)
	(. exfat_obj1 :allocate_cluster)

	; Clone 1
	(defq fs_stream2 (memory-stream))
	(when-bind (exfat_obj2 (clone_image exfat_obj1 fs_stream2))

		; Clone 2 (from clone 1)
		(defq fs_stream3 (memory-stream))
		(when-bind (exfat_obj3 (clone_image exfat_obj2 fs_stream3))

			; All three should be identical
			(assert_true "Original and clone 1 match"
				(compare_images exfat_obj1 exfat_obj2))

			(assert_true "Clone 1 and clone 2 match"
				(compare_images exfat_obj2 exfat_obj3))

			(assert_true "Original and clone 2 match"
				(compare_images exfat_obj1 exfat_obj3)))))

(defun test_clone_isolation ()
	; Test that clones are independent
	(print)
	(print "Testing clone isolation...")

	; Create and clone
	(defq fs_stream1 (memory-stream)
		exfat_obj1 (ExFat fs_stream1)
		fs_size (* 5 1024 1024))

	(. exfat_obj1 :format fs_size)

	(defq fs_stream2 (memory-stream))
	(when-bind (exfat_obj2 (clone_image exfat_obj1 fs_stream2))

		; Modify original
		(defq cluster_orig (. exfat_obj1 :allocate_cluster))

		; Modify clone
		(defq cluster_clone (. exfat_obj2 :allocate_cluster))

		; They should allocate the same cluster number but be independent
		(assert_equal "Both allocate same logical cluster"
			cluster_orig cluster_clone)

		; But modifications don't affect each other
		(. exfat_obj1 :allocate_cluster)
		(. exfat_obj1 :allocate_cluster)

		; Clone should still have more free clusters
		(assert_true "Clones are independent"
			(not (compare_images exfat_obj1 exfat_obj2)))))

(defun main ()
	(print "ExFat Image Tool Test Suite")
	(print "============================")

	; Run all tests
	(test_stream_cloning)
	(test_image_comparison)
	(test_clone_preserves_data)
	(test_clone_with_data_writes)
	(test_chunked_io_efficiency)
	(test_empty_filesystem_clone)
	(test_multiple_clones)
	(test_clone_isolation)

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
