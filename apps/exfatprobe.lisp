;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Filesystem Detector
; Validates and reports filesystem parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defun check_boot_signature (boot_sector)
	; Check for boot sector signature 0x55AA at offset 510-511
	; inputs: boot_sector = boot sector data (at least 512 bytes)
	; outputs: :t if valid, :nil otherwise
	(when (>= (length boot_sector) 512)
		(defq sig_byte0 (code boot_sector 1 510)
			sig_byte1 (code boot_sector 1 511))
		(and (= sig_byte0 0x55) (= sig_byte1 0xAA))))

(defun check_filesystem_name (boot_sector)
	; Check for "EXFAT   " identifier at offset 3-10
	; inputs: boot_sector = boot sector data
	; outputs: :t if valid, :nil otherwise
	(when (>= (length boot_sector) 11)
		(defq fs_name (slice boot_sector 3 11))
		(= fs_name "EXFAT   ")))

(defun check_jump_boot (boot_sector)
	; Check jump boot instruction (0xEB 0x76 0x90)
	; inputs: boot_sector = boot sector data
	; outputs: :t if valid, :nil otherwise
	(when (>= (length boot_sector) 3)
		(defq byte0 (code boot_sector 1 0)
			byte1 (code boot_sector 1 1)
			byte2 (code boot_sector 1 2))
		(and (= byte0 0xEB) (= byte1 0x76) (= byte2 0x90))))

(defun read_uint32_le (data offset)
	; Read 32-bit little-endian integer
	; inputs: data = byte array, offset = start position
	; outputs: 32-bit integer value
	(when (>= (length data) (+ offset 4))
		(+ (code data 1 offset)
			(<< (code data 1 (+ offset 1)) 8)
			(<< (code data 1 (+ offset 2)) 16)
			(<< (code data 1 (+ offset 3)) 24))))

(defun read_uint64_le (data offset)
	; Read 64-bit little-endian integer
	; inputs: data = byte array, offset = start position
	; outputs: 64-bit integer value
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
	; Extract key filesystem parameters from boot sector
	; inputs: boot_sector = boot sector data
	; outputs: property list with parameters or :nil
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

		; Calculate derived values
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
	; Validate filesystem parameters are reasonable
	; inputs: params = parameter property list
	; outputs: :t if valid, :nil with error messages otherwise
	(defq valid :t)

	; Check bytes per sector is valid (512, 1024, 2048, 4096)
	(defq bps (get params :bytes_per_sector))
	(unless (or (= bps 512) (= bps 1024) (= bps 2048) (= bps 4096))
		(print "Error: Invalid bytes per sector: " bps)
		(setq valid :nil))

	; Check sectors per cluster is power of 2 and reasonable (1-256)
	(defq spc (get params :sectors_per_cluster))
	(unless (and (>= spc 1) (<= spc 256))
		(print "Error: Invalid sectors per cluster: " spc)
		(setq valid :nil))

	; Check cluster count is positive
	(defq cc (get params :cluster_count))
	(unless (> cc 0)
		(print "Error: Invalid cluster count: " cc)
		(setq valid :nil))

	; Check FAT offset is after boot sector
	(defq fo (get params :fat_offset))
	(unless (>= fo 24)  ; ExFat spec: FAT starts at sector 24 or later
		(print "Error: Invalid FAT offset: " fo)
		(setq valid :nil))

	; Check cluster heap offset is after FAT
	(defq cho (get params :cluster_heap_offset)
		fl (get params :fat_length))
	(unless (>= cho (+ fo fl))
		(print "Error: Cluster heap offset must be after FAT")
		(setq valid :nil))

	; Check root cluster is valid (>= 2)
	(defq rc (get params :root_cluster))
	(unless (>= rc 2)
		(print "Error: Invalid root cluster: " rc)
		(setq valid :nil))

	valid)

(defun probe_exfat_stream (stream)
	; Probe a stream for ExFat filesystem
	; inputs: stream = stream object to probe
	; outputs: parameter list if valid, :nil otherwise
	(stream-seek stream 0 0)
	(when-bind (boot_sector (read-blk stream 512))
		; Check basic signatures
		(unless (check_boot_signature boot_sector)
			(print "Error: Invalid boot signature")
			(return :nil))

		(unless (check_jump_boot boot_sector)
			(print "Error: Invalid jump boot instruction")
			(return :nil))

		(unless (check_filesystem_name boot_sector)
			(print "Error: Not an ExFat filesystem")
			(return :nil))

		; Extract and validate parameters
		(when-bind (params (extract_filesystem_parameters boot_sector))
			(if (validate_parameters params)
				params
				:nil))))

(defun report_filesystem_info (params)
	; Print detailed filesystem information
	; inputs: params = parameter property list
	(print)
	(print "ExFat Filesystem Detected")
	(print "=========================")
	(print)
	(print "Geometry:")
	(print "  Bytes per sector:       " (get params :bytes_per_sector))
	(print "  Sectors per cluster:    " (get params :sectors_per_cluster))
	(print "  Bytes per cluster:      " (get params :bytes_per_cluster))
	(print)
	(print "Layout:")
	(print "  Volume length:          " (get params :volume_length) " sectors")
	(print "  Volume size:            " (get params :volume_size_bytes) " bytes")
	(print "  FAT offset:             " (get params :fat_offset) " sectors")
	(print "  FAT length:             " (get params :fat_length) " sectors")
	(print "  FAT size:               " (get params :fat_size_bytes) " bytes")
	(print "  Cluster heap offset:    " (get params :cluster_heap_offset) " sectors")
	(print "  Cluster count:          " (get params :cluster_count))
	(print "  Cluster heap size:      " (get params :cluster_heap_size_bytes) " bytes")
	(print)
	(print "Volume:")
	(print "  Root directory cluster: " (get params :root_cluster))
	(print "  Volume serial number:   0x" (str-from-num (get params :volume_serial) 16 8))
	(print "  Filesystem revision:    " (get params :fs_revision))
	(print "  Volume flags:           0x" (str-from-num (get params :volume_flags) 16 2)))

(defun main ()
	(print "ExFat Filesystem Detector")
	(print "=========================")
	(print)

	; Create demo filesystem
	(print "Creating demo filesystem for validation...")
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 10 1024 1024))

	(. exfat_obj :format fs_size)
	(print "Demo filesystem created (10 MB)")
	(print)

	; Probe the filesystem
	(print "Probing filesystem...")
	(when-bind (params (probe_exfat_stream fs_stream))
		(print "Validation successful!")
		(report_filesystem_info params)
		(print)
		(print "ExFat filesystem is valid"))

	(print)
	(print "Probe Demo Complete")
	(print)
	(print "Usage in real application:")
	(print "  ; Probe a stream")
	(print "  (when-bind (params (probe_exfat_stream my_stream))")
	(print "    (report_filesystem_info params))")

	0)

; Run the demo
(main)
