;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Filesystem Test/Demo
; Demonstrates using the ExFat filesystem in a memory-stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defun main ()
	(print "ExFat Filesystem Test")
	(print "=====================")
	(print)

	; Create a memory stream for the filesystem
	(print "Creating memory stream for filesystem image...")
	(defq fs_stream (memory-stream))

	; Create ExFat filesystem instance
	(print "Creating ExFat filesystem instance...")
	(defq exfat_obj (ExFat fs_stream))

	; Format the filesystem (10 MB)
	(defq fs_size (* 10 1024 1024))
	(print "Formatting filesystem (" fs_size " bytes = " (/ fs_size 1024 1024) " MB)...")
	(. exfat_obj :format fs_size)
	(print "Format complete!")
	(print)

	; Print filesystem info
	(print "Filesystem Information:")
	(print "  Sector size: " (get exfat_obj :sector_size) " bytes")
	(print "  Cluster size: " (get exfat_obj :cluster_size) " bytes")
	(print "  Cluster count: " (get exfat_obj :cluster_count))
	(print "  FAT offset: " (get exfat_obj :fat_offset) " sectors")
	(print "  FAT length: " (get exfat_obj :fat_length) " sectors")
	(print "  Cluster heap offset: " (get exfat_obj :cluster_heap_offset) " sectors")
	(print "  Root directory cluster: " (get exfat_obj :root_dir_cluster))
	(print "  Total size: " (. exfat_obj :get_size) " bytes")
	(print)

	; Test mounting
	(print "Testing mount operation...")
	(. exfat_obj :unmount)
	(if (. exfat_obj :mount)
		(print "Mount successful!")
		(print "Mount failed!"))
	(print)

	; Test file creation
	(print "Creating test file...")
	(if (. exfat_obj :create "/test.txt")
		(print "File created successfully!")
		(print "File creation failed!"))
	(print)

	; Test file operations
	(print "Testing file operations...")
	(when-bind (file_handle (. exfat_obj :open "/test.txt" :write))
		(print "File opened for writing")

		; Write some data
		(defq test_data "Hello, ExFat filesystem! This is a test.")
		(defq bytes_written (. exfat_obj :write file_handle test_data))
		(print "Wrote " bytes_written " bytes: '" test_data "'")

		; Close the file
		(. exfat_obj :close file_handle)
		(print "File closed"))
	(print)

	; Test reading
	(print "Testing file read...")
	(when-bind (file_handle (. exfat_obj :open "/test.txt" :read))
		(print "File opened for reading")

		; Seek to beginning
		(. exfat_obj :seek file_handle 0 0)

		; Read data
		(when-bind (read_data (. exfat_obj :read file_handle nil 100))
			(print "Read " (length read_data) " bytes: '" read_data "'"))

		; Close the file
		(. exfat_obj :close file_handle)
		(print "File closed"))
	(print)

	; Test directory creation
	(print "Creating test directory...")
	(if (. exfat_obj :mkdir "/testdir")
		(print "Directory created successfully!")
		(print "Directory creation failed!"))
	(print)

	; Test FAT operations
	(print "Testing FAT operations...")
	(print "  FAT entry 0: " (str-from-num (. exfat_obj :read_fat_entry 0) 16))
	(print "  FAT entry 1: " (str-from-num (. exfat_obj :read_fat_entry 1) 16))
	(print "  FAT entry 2 (root): " (str-from-num (. exfat_obj :read_fat_entry 2) 16))
	(print)

	; Test cluster allocation
	(print "Testing cluster allocation...")
	(when-bind (new_cluster (. exfat_obj :allocate_cluster))
		(print "Allocated cluster: " new_cluster)
		(print "FAT entry for new cluster: " (str-from-num (. exfat_obj :read_fat_entry new_cluster) 16))

		; Write some data to the cluster
		(defq cluster_data "This is test data written directly to a cluster.")
		(. exfat_obj :write_cluster new_cluster cluster_data)
		(print "Wrote data to cluster " new_cluster)

		; Read it back
		(when-bind (read_back_data (. exfat_obj :read_cluster new_cluster))
			(print "Read back from cluster: '" (slice read_back_data 0 (length cluster_data)) "'"))

		; Free the cluster
		(. exfat_obj :free_cluster_chain new_cluster)
		(print "Freed cluster chain starting at " new_cluster)
		(print "FAT entry after free: " (str-from-num (. exfat_obj :read_fat_entry new_cluster) 16)))
	(print)

	; Get the raw filesystem image
	(print "Filesystem Image Information:")
	(print "  Stream type: " (class-name fs_stream))
	(stream-flush fs_stream)
	(print "  Stream position: " (stream-seek fs_stream 0 1) " bytes")
	(print)
	(print "The filesystem image is stored in the memory-stream and could be")
	(print "written to physical media via a raw block driver.")
	(print)

	; Unmount
	(print "Unmounting filesystem...")
	(. exfat_obj :unmount)
	(print "Done!")
	(print)

	; Cleanup
	(print "Note: In a real application, the memory-stream data could be:")
	(print "  1. Written to a file using file-stream")
	(print "  2. Sent to a block device driver")
	(print "  3. Mounted on a host OS (if written to physical media)")
	(print)

	; Exit
	0)

; Run the test
(main)
