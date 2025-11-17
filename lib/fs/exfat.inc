;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Filesystem Implementation
; Implements ExFat filesystem inside a memory-stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/fs.inc")
(import "class/obj/lisp.inc")

;;;;;;;;;;;;;;;;;;;;
; ExFat Constants
;;;;;;;;;;;;;;;;;;;;

; Sector and cluster sizes
(defcvar +exfat_sector_size 512)
(defcvar +exfat_default_cluster_shift 12)  ; 4096 bytes per cluster (2^12)
(defcvar +exfat_boot_sectors 12)           ; VBR + extended boot sectors + OEM params
(defcvar +exfat_fat_offset 24)             ; FAT starts at sector 24

; Directory entry types
(defcvar +exfat_entry_eod 0x00)            ; End of directory
(defcvar +exfat_entry_alloc_bitmap 0x81)   ; Allocation bitmap
(defcvar +exfat_entry_upcase_table 0x82)   ; Up-case table
(defcvar +exfat_entry_volume_label 0x83)   ; Volume label
(defcvar +exfat_entry_file 0x85)           ; File entry
(defcvar +exfat_entry_stream_ext 0xC0)     ; Stream extension
(defcvar +exfat_entry_file_name 0xC1)      ; File name extension

; File attributes
(defcvar +exfat_attr_read_only 0x01)
(defcvar +exfat_attr_hidden 0x02)
(defcvar +exfat_attr_system 0x04)
(defcvar +exfat_attr_directory 0x10)
(defcvar +exfat_attr_archive 0x20)

; FAT entry values
(defcvar +exfat_fat_free 0x00000000)
(defcvar +exfat_fat_bad 0xFFFFFFF7)
(defcvar +exfat_fat_eoc 0xFFFFFFFF)        ; End of cluster chain

; Directory entry size
(defcvar +exfat_entry_size 32)

;;;;;;;;;;;;;;;;;;;;
; ExFat Class
;;;;;;;;;;;;;;;;;;;;

(defclass ExFat (Fs)
	; ExFat filesystem implementation
	; Uses a memory-stream to store the complete filesystem image

	(defmethod :init (stream)
		; Initialize ExFat filesystem with a stream
		; inputs
		; stream = stream object (typically memory-stream)
		(super :init stream)
		(setd this :mounted nil
			:sector-size +exfat_sector_size
			:cluster-shift +exfat_default_cluster_shift
			:cluster-size (<< 1 +exfat_default_cluster_shift)
			:fat-offset +exfat_fat_offset
			:fat-length 0
			:cluster-heap-offset 0
			:cluster-count 0
			:root-dir-cluster 0
			:open-files (list))
		this)

	;;;;;;;;;;;;;;;;;;;;
	; Helper Methods
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :read-sector (sector-num)
		; Read a sector from the stream
		; inputs
		; sector-num = sector number
		; outputs
		; str containing sector data, or nil on error
		(defq stream (get this :stream)
			sector-size (get this :sector-size)
			offset (* sector-num sector-size))
		(when (stream-seek stream offset 0)
			(read-blk stream sector-size)))

	(defmethod :write-sector (sector-num data)
		; Write a sector to the stream
		; inputs
		; sector-num = sector number
		; data = data to write (str)
		; outputs
		; t if successful, nil otherwise
		(defq stream (get this :stream)
			sector-size (get this :sector-size)
			offset (* sector-num sector-size))
		(when (stream-seek stream offset 0)
			(write-blk stream data)
			t))

	(defmethod :read-cluster (cluster-num)
		; Read a cluster from the stream
		; inputs
		; cluster-num = cluster number (0-based in cluster heap)
		; outputs
		; str containing cluster data, or nil on error
		(defq cluster-size (get this :cluster-size)
			cluster-heap-offset (get this :cluster-heap-offset)
			sector-size (get this :sector-size)
			cluster-shift (get this :cluster-shift)
			; Convert cluster number to sector number
			sectors-per-cluster (>> cluster-size (logn 2 sector-size))
			sector-num (+ cluster-heap-offset (* (- cluster-num 2) sectors-per-cluster)))
		(when (>= cluster-num 2)
			; Read all sectors in the cluster
			(defq result (str-alloc cluster-size))
			(each! 0 sectors-per-cluster
				(lambda (i)
					(defq sector-data (. this :read-sector (+ sector-num i)))
					(when sector-data
						; Copy sector data into result
						(str-cpy sector-data 0 result (* i sector-size) (min sector-size (- cluster-size (* i sector-size)))))))
			result))

	(defmethod :write-cluster (cluster-num data)
		; Write a cluster to the stream
		; inputs
		; cluster-num = cluster number
		; data = data to write (str, will be padded to cluster size)
		; outputs
		; t if successful, nil otherwise
		(defq cluster-size (get this :cluster-size)
			cluster-heap-offset (get this :cluster-heap-offset)
			sector-size (get this :sector-size)
			cluster-shift (get this :cluster-shift)
			sectors-per-cluster (>> cluster-size (logn 2 sector-size))
			sector-num (+ cluster-heap-offset (* (- cluster-num 2) sectors-per-cluster)))
		(when (>= cluster-num 2)
			; Pad data to cluster size
			(when (< (length data) cluster-size)
				(setq data (cat data (str-alloc (- cluster-size (length data))))))
			; Write all sectors in the cluster
			(defq success t)
			(each! 0 sectors-per-cluster
				(lambda (i)
					(defq offset (* i sector-size)
						sector-data (slice data offset (+ offset sector-size)))
					(unless (. this :write-sector (+ sector-num i) sector-data)
						(setq success nil))))
			success))

	(defmethod :read-fat-entry (cluster-num)
		; Read a FAT entry
		; inputs
		; cluster-num = cluster number
		; outputs
		; FAT entry value (next cluster in chain, or special value)
		(defq fat-offset (get this :fat-offset)
			sector-size (get this :sector-size)
			; Each FAT entry is 4 bytes (32-bit)
			fat-byte-offset (* cluster-num 4)
			sector-num (+ fat-offset (/ fat-byte-offset sector-size))
			offset-in-sector (% fat-byte-offset sector-size))
		(when-bind (sector-data (. this :read-sector sector-num))
			; Extract 4-byte FAT entry (little-endian)
			(+ (code sector-data offset-in-sector)
				(<< (code sector-data (+ offset-in-sector 1)) 8)
				(<< (code sector-data (+ offset-in-sector 2)) 16)
				(<< (code sector-data (+ offset-in-sector 3)) 24))))

	(defmethod :write-fat-entry (cluster-num value)
		; Write a FAT entry
		; inputs
		; cluster-num = cluster number
		; value = FAT entry value
		; outputs
		; t if successful, nil otherwise
		(defq fat-offset (get this :fat-offset)
			sector-size (get this :sector-size)
			fat-byte-offset (* cluster-num 4)
			sector-num (+ fat-offset (/ fat-byte-offset sector-size))
			offset-in-sector (% fat-byte-offset sector-size))
		(when-bind (sector-data (. this :read-sector sector-num))
			; Update FAT entry (little-endian)
			(setf-code sector-data offset-in-sector (& value 0xFF))
			(setf-code sector-data (+ offset-in-sector 1) (& (>> value 8) 0xFF))
			(setf-code sector-data (+ offset-in-sector 2) (& (>> value 16) 0xFF))
			(setf-code sector-data (+ offset-in-sector 3) (& (>> value 24) 0xFF))
			(. this :write-sector sector-num sector-data)))

	(defmethod :allocate-cluster ()
		; Allocate a new cluster from the FAT
		; outputs
		; cluster number, or nil if no free clusters
		(defq cluster-count (get this :cluster-count))
		; Start from cluster 2 (0 and 1 are reserved)
		(some! 2 cluster-count
			(lambda (cluster)
				(when (= (. this :read-fat-entry cluster) +exfat_fat_free)
					; Mark cluster as end of chain
					(. this :write-fat-entry cluster +exfat_fat_eoc)
					cluster))))

	(defmethod :free-cluster-chain (start-cluster)
		; Free a chain of clusters
		; inputs
		; start-cluster = first cluster in chain
		; outputs
		; t if successful
		(when (>= start-cluster 2)
			(defq current-cluster start-cluster)
			(while (and current-cluster (>= current-cluster 2) (< current-cluster +exfat_fat_eoc))
				(defq next-cluster (. this :read-fat-entry current-cluster))
				(. this :write-fat-entry current-cluster +exfat_fat_free)
				(setq current-cluster next-cluster)))
		t)

	;;;;;;;;;;;;;;;;;;;;
	; Filesystem Operations
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :format (size)
		; Format the filesystem with the given size
		; inputs
		; size = total size in bytes
		; outputs
		; t if successful, nil otherwise
		(defq stream (get this :stream)
			sector-size (get this :sector-size)
			cluster-shift (get this :cluster-shift)
			cluster-size (get this :cluster-size)
			total-sectors (/ size sector-size)
			fat-offset +exfat_fat_offset)

		; Calculate FAT size (32-bit entries, 1 entry per cluster)
		(defq cluster-heap-start (+ fat-offset 128)  ; Reserve 64KB for FAT
			cluster-count (/ (- total-sectors cluster-heap-start) (>> cluster-size (logn 2 sector-size)))
			fat-length (/ (+ (* cluster-count 4) sector-size -1) sector-size))  ; Round up

		; Update filesystem parameters
		(setd this :fat-offset fat-offset
			:fat-length fat-length
			:cluster-heap-offset (+ fat-offset fat-length)
			:cluster-count cluster-count
			:root-dir-cluster 2
			:mounted nil)

		; Write boot sector
		(. this :write-boot-sector size)

		; Initialize FAT (all entries free except reserved and root)
		(each! 0 fat-length
			(lambda (i)
				(. this :write-sector (+ fat-offset i) (str-alloc sector-size))))

		; Mark first two FAT entries as reserved
		(. this :write-fat-entry 0 0xFFFFFFF8)  ; Media type
		(. this :write-fat-entry 1 +exfat_fat_eoc)

		; Mark root directory cluster as end of chain
		(. this :write-fat-entry 2 +exfat_fat_eoc)

		; Initialize root directory (empty)
		(. this :write-cluster 2 (cat
			; Allocation bitmap entry
			(. this :make-alloc-bitmap-entry)
			; End of directory marker
			(str-alloc (- cluster-size +exfat_entry_size))))

		(setd this :mounted t)
		t)

	(defmethod :write-boot-sector (volume-size)
		; Write the ExFat boot sector
		; inputs
		; volume-size = total volume size in bytes
		(defq sector-size (get this :sector-size)
			cluster-shift (get this :cluster-shift)
			fat-offset (get this :fat-offset)
			fat-length (get this :fat-length)
			cluster-heap-offset (get this :cluster-heap-offset)
			cluster-count (get this :cluster-count)
			root-cluster (get this :root-dir-cluster)
			boot-sector (str-alloc sector-size))

		; Jump boot code (3 bytes)
		(setf-code boot-sector 0 0xEB)
		(setf-code boot-sector 1 0x76)
		(setf-code boot-sector 2 0x90)

		; File system name "EXFAT   " (8 bytes)
		(str-cpy "EXFAT   " 0 boot-sector 3 8)

		; Zeros (53 bytes) - must be zero
		(each! 11 64 (lambda (i) (setf-code boot-sector i 0)))

		; Partition offset (8 bytes) - always 0 for our purposes
		(. this :write-u64-le boot-sector 64 0)

		; Volume length (8 bytes) - total sectors
		(. this :write-u64-le boot-sector 72 (/ volume-size sector-size))

		; FAT offset (4 bytes)
		(. this :write-u32-le boot-sector 80 fat-offset)

		; FAT length (4 bytes)
		(. this :write-u32-le boot-sector 84 fat-length)

		; Cluster heap offset (4 bytes)
		(. this :write-u32-le boot-sector 88 cluster-heap-offset)

		; Cluster count (4 bytes)
		(. this :write-u32-le boot-sector 92 cluster-count)

		; Root directory first cluster (4 bytes)
		(. this :write-u32-le boot-sector 96 root-cluster)

		; Volume serial number (4 bytes)
		(. this :write-u32-le boot-sector 100 0x12345678)

		; File system revision (2 bytes) - version 1.0
		(setf-code boot-sector 104 0x00)
		(setf-code boot-sector 105 0x01)

		; Volume flags (2 bytes)
		(setf-code boot-sector 106 0x00)
		(setf-code boot-sector 107 0x00)

		; Bytes per sector shift (1 byte) - 9 for 512 bytes
		(setf-code boot-sector 108 9)

		; Sectors per cluster shift (1 byte)
		(setf-code boot-sector 109 (- cluster-shift 9))

		; Number of FATs (1 byte) - always 1
		(setf-code boot-sector 110 1)

		; Drive select (1 byte)
		(setf-code boot-sector 111 0x80)

		; Percent in use (1 byte)
		(setf-code boot-sector 112 0)

		; Reserved (7 bytes)
		(each! 113 120 (lambda (i) (setf-code boot-sector i 0)))

		; Boot signature (2 bytes)
		(setf-code boot-sector 510 0x55)
		(setf-code boot-sector 511 0xAA)

		; Write boot sector
		(. this :write-sector 0 boot-sector))

	(defmethod :write-u32-le (buffer offset value)
		; Write 32-bit little-endian value
		(setf-code buffer offset (& value 0xFF))
		(setf-code buffer (+ offset 1) (& (>> value 8) 0xFF))
		(setf-code buffer (+ offset 2) (& (>> value 16) 0xFF))
		(setf-code buffer (+ offset 3) (& (>> value 24) 0xFF)))

	(defmethod :write-u64-le (buffer offset value)
		; Write 64-bit little-endian value
		(. this :write-u32-le buffer offset (& value 0xFFFFFFFF))
		(. this :write-u32-le buffer (+ offset 4) (>> value 32)))

	(defmethod :read-u32-le (buffer offset)
		; Read 32-bit little-endian value
		(+ (code buffer offset)
			(<< (code buffer (+ offset 1)) 8)
			(<< (code buffer (+ offset 2)) 16)
			(<< (code buffer (+ offset 3)) 24)))

	(defmethod :make-alloc-bitmap-entry ()
		; Create allocation bitmap directory entry
		(defq entry (str-alloc +exfat_entry_size))
		(setf-code entry 0 +exfat_entry_alloc_bitmap)
		; Flags and reserved bytes
		(each! 1 +exfat_entry_size (lambda (i) (setf-code entry i 0)))
		entry)

	(defmethod :mount ()
		; Mount the filesystem
		; outputs
		; t if successful, nil otherwise
		(when-bind (boot-sector (. this :read-sector 0))
			; Verify boot signature
			(when (and (= (code boot-sector 510) 0x55)
					(= (code boot-sector 511) 0xAA))
				; Read filesystem parameters
				(defq fat-offset (. this :read-u32-le boot-sector 80)
					fat-length (. this :read-u32-le boot-sector 84)
					cluster-heap-offset (. this :read-u32-le boot-sector 88)
					cluster-count (. this :read-u32-le boot-sector 92)
					root-cluster (. this :read-u32-le boot-sector 96)
					sectors-per-cluster-shift (code boot-sector 109))

				(setd this :fat-offset fat-offset
					:fat-length fat-length
					:cluster-heap-offset cluster-heap-offset
					:cluster-count cluster-count
					:root-dir-cluster root-cluster
					:cluster-shift (+ sectors-per-cluster-shift 9)
					:cluster-size (<< 1 (+ sectors-per-cluster-shift 9))
					:mounted t)
				t)))

	(defmethod :unmount ()
		; Unmount the filesystem
		(stream-flush (get this :stream))
		(setd this :mounted nil)
		t)

	;;;;;;;;;;;;;;;;;;;;
	; Directory Operations
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :find-entry (dir-cluster name)
		; Find a directory entry by name
		; inputs
		; dir-cluster = directory cluster number
		; name = entry name to find (str)
		; outputs
		; (list entry-cluster entry-offset) or nil if not found
		(defq entries (. this :read-directory dir-cluster))
		(when entries
			(some! 0 (length entries)
				(lambda (i)
					(defq entry (elem entries i))
					(when (and entry (= (get entry :name) name))
						(list dir-cluster (* i +exfat_entry_size)))))))

	(defmethod :read-directory (cluster-num)
		; Read and parse directory entries
		; inputs
		; cluster-num = directory cluster number
		; outputs
		; list of entry hash-maps, or nil on error
		(when-bind (cluster-data (. this :read-cluster cluster-num))
			(defq entries (list)
				offset 0
				cluster-size (get this :cluster-size))

			; Parse directory entries
			(while (< offset cluster-size)
				(defq entry-type (code cluster-data offset))
				(cond
					; End of directory
					((= entry-type +exfat_entry_eod)
						(break))
					; File entry
					((= entry-type +exfat_entry_file)
						(defq file-entry (. this :parse-file-entry cluster-data offset))
						(when file-entry
							(push entries file-entry)))
					; Other entries (skip for now)
					(t nil))
				(setq offset (+ offset +exfat_entry_size)))
			entries))

	(defmethod :parse-file-entry (data offset)
		; Parse a file directory entry (simplified)
		; inputs
		; data = cluster data
		; offset = offset to file entry
		; outputs
		; hash-map with file info, or nil
		(defq entry-type (code data offset))
		(when (= entry-type +exfat_entry_file)
			(defq secondary-count (code data (+ offset 1))
				attributes (. this :read-u16-le data (+ offset 4)))
			; For now, create a basic entry
			; In full implementation, would read stream extension and file name entries
			(hash :type (if (bit? attributes +exfat_attr_directory) :dir :file)
				:attributes attributes
				:name "unnamed"  ; Would read from file name entry
				:size 0          ; Would read from stream extension
				:cluster 0)))    ; Would read from stream extension

	(defmethod :read-u16-le (buffer offset)
		; Read 16-bit little-endian value
		(+ (code buffer offset)
			(<< (code buffer (+ offset 1)) 8)))

	(defmethod :mkdir (path)
		; Create a directory
		; inputs
		; path = directory path (str)
		; outputs
		; t if successful, nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Allocate cluster for new directory
		(when-bind (new-cluster (. this :allocate-cluster))
			; Initialize empty directory
			(. this :write-cluster new-cluster (str-alloc (get this :cluster-size)))

			; Add entry to parent directory (simplified - assumes root)
			(defq root-cluster (get this :root-dir-cluster))
			; TODO: Add proper directory entry creation
			t))

	(defmethod :create (path)
		; Create a file
		; inputs
		; path = file path (str)
		; outputs
		; t if successful, nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Allocate cluster for file data
		(when-bind (new-cluster (. this :allocate-cluster))
			; Initialize empty file
			(. this :write-cluster new-cluster (str-alloc (get this :cluster-size)))

			; Add entry to parent directory (simplified - assumes root)
			(defq root-cluster (get this :root-dir-cluster))
			; TODO: Add proper file entry creation
			t))

	;;;;;;;;;;;;;;;;;;;;
	; File Operations
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :open (path mode)
		; Open a file
		; inputs
		; path = file path (str)
		; mode = open mode (:read, :write, :append)
		; outputs
		; file handle (hash-map) or nil
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Create file handle
		(defq handle (hash :path path
			:mode mode
			:cluster 0
			:position 0
			:size 0))

		; Add to open files list
		(push (get this :open-files) handle)
		handle)

	(defmethod :close (handle)
		; Close a file
		; inputs
		; handle = file handle
		; outputs
		; t if successful
		(defq open-files (get this :open-files))
		(setd this :open-files (filter (lambda (h) (/= h handle)) open-files))
		t)

	(defmethod :read (handle buffer size)
		; Read from a file
		; inputs
		; handle = file handle
		; buffer = buffer (ignored, always allocates new)
		; size = bytes to read
		; outputs
		; str with data, or nil on error
		(defq cluster (get handle :cluster)
			position (get handle :position))
		(when (and cluster (> cluster 0))
			(when-bind (cluster-data (. this :read-cluster cluster))
				(defq available (- (length cluster-data) position)
					to-read (min size available))
				(when (> to-read 0)
					(setd handle :position (+ position to-read))
					(slice cluster-data position (+ position to-read))))))

	(defmethod :write (handle data)
		; Write to a file
		; inputs
		; handle = file handle
		; data = data to write (str)
		; outputs
		; bytes written, or -1 on error
		(defq cluster (get handle :cluster))
		(if (and cluster (> cluster 0))
			(progn
				(. this :write-cluster cluster data)
				(setd handle :position (length data))
				(length data))
			-1))

	(defmethod :seek (handle offset whence)
		; Seek in a file
		; inputs
		; handle = file handle
		; offset = offset
		; whence = 0 (start), 1 (current), 2 (end)
		; outputs
		; new position
		(defq position (get handle :position)
			size (get handle :size))
		(defq new-pos (cond
			((= whence 0) offset)
			((= whence 1) (+ position offset))
			((= whence 2) (+ size offset))
			(t position)))
		(setd handle :position (max 0 new-pos))
		new-pos)

	(defmethod :tell (handle)
		; Get current position
		(get handle :position))

	(defmethod :remove (path)
		; Remove a file
		; inputs
		; path = file path (str)
		; outputs
		; t if successful
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement file removal
		t)

	(defmethod :rmdir (path)
		; Remove a directory
		; inputs
		; path = directory path (str)
		; outputs
		; t if successful
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement directory removal
		t)

	(defmethod :exists (path)
		; Check if path exists
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement existence check
		nil)

	(defmethod :stat (path)
		; Get file/directory info
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement stat
		nil)

	(defmethod :list (path)
		; List directory contents
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		(defq root-cluster (get this :root-dir-cluster)
			entries (. this :read-directory root-cluster))
		(when entries
			(map (lambda (entry) (get entry :name)) entries)))

	(defmethod :rename (old-path new-path)
		; Rename/move file or directory
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement rename
		t)

	(defmethod :get-size ()
		; Get filesystem size
		; outputs
		; total size in bytes
		(* (get this :cluster-count) (get this :cluster-size))))
