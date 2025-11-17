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
	(defq fs-stream (memory-stream))

	; Create ExFat filesystem instance
	(prinl "Creating ExFat filesystem instance...")
	(defq exfat (ExFat fs-stream))

	; Format the filesystem (10 MB)
	(defq fs-size (* 10 1024 1024))
	(prinl "Formatting filesystem (" fs-size " bytes = " (/ fs-size 1024 1024) " MB)...")
	(. exfat :format fs-size)
	(prinl "Format complete!")
	(prinl)

	; Print filesystem info
	(prinl "Filesystem Information:")
	(prinl "  Sector size: " (get exfat :sector-size) " bytes")
	(prinl "  Cluster size: " (get exfat :cluster-size) " bytes")
	(prinl "  Cluster count: " (get exfat :cluster-count))
	(prinl "  FAT offset: " (get exfat :fat-offset) " sectors")
	(prinl "  FAT length: " (get exfat :fat-length) " sectors")
	(prinl "  Cluster heap offset: " (get exfat :cluster-heap-offset) " sectors")
	(prinl "  Root directory cluster: " (get exfat :root-dir-cluster))
	(prinl "  Total size: " (. exfat :get-size) " bytes")
	(prinl)

	; Test mounting
	(prinl "Testing mount operation...")
	(. exfat :unmount)
	(if (. exfat :mount)
		(prinl "Mount successful!")
		(prinl "Mount failed!"))
	(prinl)

	; Test file creation
	(prinl "Creating test file...")
	(if (. exfat :create "/test.txt")
		(prinl "File created successfully!")
		(prinl "File creation failed!"))
	(prinl)

	; Test file operations
	(prinl "Testing file operations...")
	(when-bind (handle (. exfat :open "/test.txt" :write))
		(prinl "File opened for writing")

		; Write some data
		(defq test-data "Hello, ExFat filesystem! This is a test.")
		(defq bytes-written (. exfat :write handle test-data))
		(prinl "Wrote " bytes-written " bytes: '" test-data "'")

		; Close the file
		(. exfat :close handle)
		(prinl "File closed"))
	(prinl)

	; Test reading
	(prinl "Testing file read...")
	(when-bind (handle (. exfat :open "/test.txt" :read))
		(prinl "File opened for reading")

		; Seek to beginning
		(. exfat :seek handle 0 0)

		; Read data
		(when-bind (data (. exfat :read handle nil 100))
			(prinl "Read " (length data) " bytes: '" data "'"))

		; Close the file
		(. exfat :close handle)
		(prinl "File closed"))
	(prinl)

	; Test directory creation
	(prinl "Creating test directory...")
	(if (. exfat :mkdir "/testdir")
		(prinl "Directory created successfully!")
		(prinl "Directory creation failed!"))
	(prinl)

	; Test FAT operations
	(prinl "Testing FAT operations...")
	(prinl "  FAT entry 0: " (str-from-num (. exfat :read-fat-entry 0) 16))
	(prinl "  FAT entry 1: " (str-from-num (. exfat :read-fat-entry 1) 16))
	(prinl "  FAT entry 2 (root): " (str-from-num (. exfat :read-fat-entry 2) 16))
	(prinl)

	; Test cluster allocation
	(prinl "Testing cluster allocation...")
	(when-bind (new-cluster (. exfat :allocate-cluster))
		(prinl "Allocated cluster: " new-cluster)
		(prinl "FAT entry for new cluster: " (str-from-num (. exfat :read-fat-entry new-cluster) 16))

		; Write some data to the cluster
		(defq cluster-data "This is test data written directly to a cluster.")
		(. exfat :write-cluster new-cluster cluster-data)
		(prinl "Wrote data to cluster " new-cluster)

		; Read it back
		(when-bind (read-data (. exfat :read-cluster new-cluster))
			(prinl "Read back from cluster: '" (slice read-data 0 (length cluster-data)) "'"))

		; Free the cluster
		(. exfat :free-cluster-chain new-cluster)
		(prinl "Freed cluster chain starting at " new-cluster)
		(prinl "FAT entry after free: " (str-from-num (. exfat :read-fat-entry new-cluster) 16)))
	(prinl)

	; Get the raw filesystem image
	(prinl "Filesystem Image Information:")
	(prinl "  Stream type: " (class-name fs-stream))
	(stream-flush fs-stream)
	(prinl "  Stream position: " (stream-seek fs-stream 0 1) " bytes")
	(prinl)
	(prinl "The filesystem image is stored in the memory-stream and could be")
	(prinl "written to physical media via a raw block driver.")
	(prinl)

	; Unmount
	(prinl "Unmounting filesystem...")
	(. exfat :unmount)
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
