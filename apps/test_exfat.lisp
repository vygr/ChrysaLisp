;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Filesystem Test/Demo
; Demonstrates using the ExFat filesystem in a memory-stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defun main ()
	(prinl "ExFat Filesystem Test")
	(prinl "=====================")
	(prinl)

	; Create a memory stream for the filesystem
	(prinl "Creating memory stream for filesystem image...")
	(defq fs_stream (memory-stream))

	; Create ExFat filesystem instance
	(prinl "Creating ExFat filesystem instance...")
	(defq exfat_obj (ExFat fs_stream))

	; Format the filesystem (10 MB)
	(defq fs_size (* 10 1024 1024))
	(prinl "Formatting filesystem (" fs_size " bytes = " (/ fs_size 1024 1024) " MB)...")
	(. exfat_obj :format fs_size)
	(prinl "Format complete!")
	(prinl)

	; Print filesystem info
	(prinl "Filesystem Information:")
	(prinl "  Sector size: " (get exfat_obj :sector_size) " bytes")
	(prinl "  Cluster size: " (get exfat_obj :cluster_size) " bytes")
	(prinl "  Cluster count: " (get exfat_obj :cluster_count))
	(prinl "  FAT offset: " (get exfat_obj :fat_offset) " sectors")
	(prinl "  FAT length: " (get exfat_obj :fat_length) " sectors")
	(prinl "  Cluster heap offset: " (get exfat_obj :cluster_heap_offset) " sectors")
	(prinl "  Root directory cluster: " (get exfat_obj :root_dir_cluster))
	(prinl "  Total size: " (. exfat_obj :get-size) " bytes")
	(prinl)

	; Test mounting
	(prinl "Testing mount operation...")
	(. exfat_obj :unmount)
	(if (. exfat_obj :mount)
		(prinl "Mount successful!")
		(prinl "Mount failed!"))
	(prinl)

	; Test file creation
	(prinl "Creating test file...")
	(if (. exfat_obj :create "/test.txt")
		(prinl "File created successfully!")
		(prinl "File creation failed!"))
	(prinl)

	; Test file operations
	(prinl "Testing file operations...")
	(when-bind (file_handle (. exfat_obj :open "/test.txt" :write))
		(prinl "File opened for writing")

		; Write some data
		(defq test_data "Hello, ExFat filesystem! This is a test.")
		(defq bytes_written (. exfat_obj :write file_handle test_data))
		(prinl "Wrote " bytes_written " bytes: '" test_data "'")

		; Close the file
		(. exfat_obj :close file_handle)
		(prinl "File closed"))
	(prinl)

	; Test reading
	(prinl "Testing file read...")
	(when-bind (file_handle (. exfat_obj :open "/test.txt" :read))
		(prinl "File opened for reading")

		; Seek to beginning
		(. exfat_obj :seek file_handle 0 0)

		; Read data
		(when-bind (read_data (. exfat_obj :read file_handle nil 100))
			(prinl "Read " (length read_data) " bytes: '" read_data "'"))

		; Close the file
		(. exfat_obj :close file_handle)
		(prinl "File closed"))
	(prinl)

	; Test directory creation
	(prinl "Creating test directory...")
	(if (. exfat_obj :mkdir "/testdir")
		(prinl "Directory created successfully!")
		(prinl "Directory creation failed!"))
	(prinl)

	; Test FAT operations
	(prinl "Testing FAT operations...")
	(prinl "  FAT entry 0: " (str-from-num (. exfat_obj :read-fat-entry 0) 16))
	(prinl "  FAT entry 1: " (str-from-num (. exfat_obj :read-fat-entry 1) 16))
	(prinl "  FAT entry 2 (root): " (str-from-num (. exfat_obj :read-fat-entry 2) 16))
	(prinl)

	; Test cluster allocation
	(prinl "Testing cluster allocation...")
	(when-bind (new_cluster (. exfat_obj :allocate-cluster))
		(prinl "Allocated cluster: " new_cluster)
		(prinl "FAT entry for new cluster: " (str-from-num (. exfat_obj :read-fat-entry new_cluster) 16))

		; Write some data to the cluster
		(defq cluster_data "This is test data written directly to a cluster.")
		(. exfat_obj :write-cluster new_cluster cluster_data)
		(prinl "Wrote data to cluster " new_cluster)

		; Read it back
		(when-bind (read_back_data (. exfat_obj :read-cluster new_cluster))
			(prinl "Read back from cluster: '" (slice read_back_data 0 (length cluster_data)) "'"))

		; Free the cluster
		(. exfat_obj :free-cluster-chain new_cluster)
		(prinl "Freed cluster chain starting at " new_cluster)
		(prinl "FAT entry after free: " (str-from-num (. exfat_obj :read-fat-entry new_cluster) 16)))
	(prinl)

	; Get the raw filesystem image
	(prinl "Filesystem Image Information:")
	(prinl "  Stream type: " (class-name fs_stream))
	(stream-flush fs_stream)
	(prinl "  Stream position: " (stream-seek fs_stream 0 1) " bytes")
	(prinl)
	(prinl "The filesystem image is stored in the memory-stream and could be")
	(prinl "written to physical media via a raw block driver.")
	(prinl)

	; Unmount
	(prinl "Unmounting filesystem...")
	(. exfat_obj :unmount)
	(prinl "Done!")
	(prinl)

	; Cleanup
	(prinl "Note: In a real application, the memory-stream data could be:")
	(prinl "  1. Written to a file using file-stream")
	(prinl "  2. Sent to a block device driver")
	(prinl "  3. Mounted on a host OS (if written to physical media)")
	(prinl)

	; Exit
	0)

; Run the test
(main)
