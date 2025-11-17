;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Filesystem Implementation
; Implements ExFat filesystem inside a memory-stream
; Uses only official ChrysaLisp stream calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/fs.inc")
(import "class/obj/lisp.inc")

;;;;;;;;;;;;;;;;;;;;
; ExFat Constants
;;;;;;;;;;;;;;;;;;;;

; Sector and cluster sizes
(defq +exfat_sector_size 512)
(defq +exfat_default_cluster_shift 12)  ; 4096 bytes per cluster (2^12)
(defq +exfat_boot_sectors 12)           ; VBR + extended boot sectors + OEM params
(defq +exfat_fat_offset 24)             ; FAT starts at sector 24

; Directory entry types
(defq +exfat_entry_eod 0x00)            ; End of directory
(defq +exfat_entry_alloc_bitmap 0x81)   ; Allocation bitmap
(defq +exfat_entry_upcase_table 0x82)   ; Up-case table
(defq +exfat_entry_volume_label 0x83)   ; Volume label
(defq +exfat_entry_file 0x85)           ; File entry
(defq +exfat_entry_stream_ext 0xC0)     ; Stream extension
(defq +exfat_entry_file_name 0xC1)      ; File name extension

; File attributes
(defq +exfat_attr_read_only 0x01)
(defq +exfat_attr_hidden 0x02)
(defq +exfat_attr_system 0x04)
(defq +exfat_attr_directory 0x10)
(defq +exfat_attr_archive 0x20)

; FAT entry values
(defq +exfat_fat_free 0x00000000)
(defq +exfat_fat_bad 0xFFFFFFF7)
(defq +exfat_fat_eoc 0xFFFFFFFF)        ; End of cluster chain

; Directory entry size
(defq +exfat_entry_size 32)

;;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;;

(defun u32-to-bytes-le (value)
	; Convert 32-bit value to little-endian bytes (4-byte string)
	(cat (char (logand value 0xFF))
		(char (logand (>> value 8) 0xFF))
		(char (logand (>> value 16) 0xFF))
		(char (logand (>> value 24) 0xFF))))

(defun u64-to-bytes-le (value)
	; Convert 64-bit value to little-endian bytes (8-byte string)
	(cat (u32-to-bytes-le (logand value 0xFFFFFFFF))
		(u32-to-bytes-le (>> value 32))))

(defun u16-to-bytes-le (value)
	; Convert 16-bit value to little-endian bytes (2-byte string)
	(cat (char (logand value 0xFF))
		(char (logand (>> value 8) 0xFF))))

(defun bytes-to-u32-le (data offset)
	; Read 32-bit little-endian value from byte string
	(+ (code data 1 offset)
		(<< (code data 1 (+ offset 1)) 8)
		(<< (code data 1 (+ offset 2)) 16)
		(<< (code data 1 (+ offset 3)) 24)))

(defun bytes-to-u16-le (data offset)
	; Read 16-bit little-endian value from byte string
	(+ (code data 1 offset)
		(<< (code data 1 (+ offset 1)) 8)))

;;;;;;;;;;;;;;;;;;;;
; ExFat Class
;;;;;;;;;;;;;;;;;;;;

(defclass ExFat (Fs)
	; ExFat filesystem implementation
	; Uses a memory-stream to store the complete filesystem image

	(defmethod :init (stream_obj)
		; Initialize ExFat filesystem with a stream
		; inputs
		; stream_obj = stream object (typically memory-stream)
		(super :init stream_obj)
		(def this :mounted :nil
			:sector_size +exfat_sector_size
			:cluster_shift +exfat_default_cluster_shift
			:cluster_size (<< 1 +exfat_default_cluster_shift)
			:fat_offset +exfat_fat_offset
			:fat_length 0
			:cluster_heap_offset 0
			:cluster_count 0
			:root_dir_cluster 0
			:open_files (list))
		this)

	;;;;;;;;;;;;;;;;;;;;
	; Helper Methods
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :read_sector (sector_num)
		; Read a sector from the stream
		; inputs
		; sector_num = sector number
		; outputs
		; str containing sector data, or :nil on error
		(defq stream_obj (get this :stream)
			sector_size (get this :sector_size)
			offset (* sector_num sector_size))
		(when (stream-seek stream_obj offset 0)
			(read-blk stream_obj sector_size)))

	(defmethod :write_sector (sector_num data)
		; Write a sector to the stream
		; inputs
		; sector_num = sector number
		; data = data to write (str)
		; outputs
		; :t if successful, :nil otherwise
		(defq stream_obj (get this :stream)
			sector_size (get this :sector_size)
			offset (* sector_num sector_size))
		(when (stream-seek stream_obj offset 0)
			; Pad or truncate to sector size
			(defq data_len (length data))
			(cond
				((< data_len sector_size)
					; Pad with zeros
					(setq data (cat data (str-alloc (- sector_size data_len)))))
				((> data_len sector_size)
					; Truncate
					(setq data (slice data 0 sector_size))))
			(write-blk stream_obj data)
		 :t))

	(defmethod :read_cluster (cluster_num)
		; Read a cluster from the stream
		; inputs
		; cluster_num = cluster number (0-based in cluster heap)
		; outputs
		; str containing cluster data, or :nil on error
		(defq cluster_size (get this :cluster_size)
			cluster_heap_offset (get this :cluster_heap_offset)
			sector_size (get this :sector_size)
			cluster_shift (get this :cluster_shift)
			; Convert cluster number to sector number
			sectors_per_cluster (>> cluster_size (log2 sector_size))
			sector_num (+ cluster_heap_offset (* (- cluster_num 2) sectors_per_cluster)))
		(when (>= cluster_num 2)
			; Read all sectors in the cluster
			(defq result_parts (list))
			(each! (lambda (i)
					(defq sector_data (. this :read_sector (+ sector_num i)))
					(when sector_data
						(push result_parts sector_data)))
				sectors_per_cluster 0)
			(when (= (length result_parts) sectors_per_cluster)
				(apply cat result_parts))))

	(defmethod :write_cluster (cluster_num data)
		; Write a cluster to the stream
		; inputs
		; cluster_num = cluster number
		; data = data to write (str, will be padded to cluster size)
		; outputs
		; :t if successful, :nil otherwise
		(defq cluster_size (get this :cluster_size)
			cluster_heap_offset (get this :cluster_heap_offset)
			sector_size (get this :sector_size)
			sectors_per_cluster (>> cluster_size (log2 sector_size))
			sector_num (+ cluster_heap_offset (* (- cluster_num 2) sectors_per_cluster)))
		(when (>= cluster_num 2)
			; Pad data to cluster size
			(defq data_len (length data))
			(when (< data_len cluster_size)
				(setq data (cat data (str-alloc (- cluster_size data_len)))))
			; Write all sectors in the cluster
			(defq success :t 				i 0)
			(while (and success (< i sectors_per_cluster))
				(defq offset (* i sector_size)
					sector_data (slice data offset (+ offset sector_size)))
				(unless (. this :write_sector (+ sector_num i) sector_data)
					(setq success :nil))
				(setq i (inc i)))
			success))

	(defmethod :read_fat_entry (cluster_num)
		; Read a FAT entry
		; inputs
		; cluster_num = cluster number
		; outputs
		; FAT entry value (next cluster in chain, or special value)
		(defq fat_offset (get this :fat_offset)
			sector_size (get this :sector_size)
			; Each FAT entry is 4 bytes (32-bit)
			fat_byte_offset (* cluster_num 4)
			sector_num (+ fat_offset (/ fat_byte_offset sector_size))
			offset_in_sector (% fat_byte_offset sector_size))
		(when-bind (sector_data (. this :read_sector sector_num))
			(bytes-to-u32-le sector_data offset_in_sector)))

	(defmethod :write_fat_entry (cluster_num value)
		; Write a FAT entry
		; inputs
		; cluster_num = cluster number
		; value = FAT entry value
		; outputs
		; :t if successful, :nil otherwise
		(defq fat_offset (get this :fat_offset)
			sector_size (get this :sector_size)
			fat_byte_offset (* cluster_num 4)
			sector_num (+ fat_offset (/ fat_byte_offset sector_size))
			offset_in_sector (% fat_byte_offset sector_size))
		(when-bind (sector_data (. this :read_sector sector_num))
			; Build new sector with updated FAT entry
			(defq new_entry (u32-to-bytes-le value)
				updated_sector (cat
					(slice sector_data 0 offset_in_sector)
					new_entry
					(slice sector_data (+ offset_in_sector 4) sector_size)))
			(. this :write_sector sector_num updated_sector)))

	(defmethod :allocate_cluster ()
		; Allocate a new cluster from the FAT
		; outputs
		; cluster number, or :nil if no free clusters
		(defq cluster_count (get this :cluster_count))
		; Start from cluster 2 (0 and 1 are reserved)
		(some! (lambda (cluster_idx)
				(when (= (. this :read_fat_entry cluster_idx) +exfat_fat_free)
					; Mark cluster as end of chain
					(. this :write_fat_entry cluster_idx +exfat_fat_eoc)
					cluster_idx))
			cluster_count :nil 2))

	(defmethod :free_cluster_chain (start_cluster)
		; Free a chain of clusters
		; inputs
		; start_cluster = first cluster in chain
		; outputs
		; :t if successful
		(when (>= start_cluster 2)
			(defq current_cluster start_cluster)
			(while (and current_cluster
					(>= current_cluster 2)
					(< current_cluster +exfat_fat_eoc))
				(defq next_cluster (. this :read_fat_entry current_cluster))
				(. this :write_fat_entry current_cluster +exfat_fat_free)
				(setq current_cluster next_cluster)))
	 :t)

	;;;;;;;;;;;;;;;;;;;;
	; Filesystem Operations
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :format (size)
		; Format the filesystem with the given size
		; inputs
		; size = total size in bytes
		; outputs
		; :t if successful, :nil otherwise
		(defq stream_obj (get this :stream)
			sector_size (get this :sector_size)
			cluster_shift (get this :cluster_shift)
			cluster_size (get this :cluster_size)
			total_sectors (/ size sector_size)
			fat_offset +exfat_fat_offset)

		; Calculate FAT size (32-bit entries, 1 entry per cluster)
		(defq cluster_heap_start (+ fat_offset 128)  ; Reserve 64KB for FAT
			cluster_count (/ (- total_sectors cluster_heap_start)
				(>> cluster_size (log2 sector_size)))
			fat_length (/ (+ (* cluster_count 4) sector_size -1) sector_size))  ; Round up

		; Update filesystem parameters
		(def this :fat_offset fat_offset
			:fat_length fat_length
			:cluster_heap_offset (+ fat_offset fat_length)
			:cluster_count cluster_count
			:root_dir_cluster 2
			:mounted :nil)

		; Write boot sector
		(. this :write_boot_sector size)

		; Initialize FAT (all entries free except reserved and root)
		(defq empty_sector (str-alloc sector_size))
		(each! (lambda (i)
				(. this :write_sector (+ fat_offset i) empty_sector))
			fat_length 0)

		; Mark first two FAT entries as reserved
		(. this :write_fat_entry 0 0xFFFFFFF8)  ; Media type
		(. this :write_fat_entry 1 +exfat_fat_eoc)

		; Mark root directory cluster as end of chain
		(. this :write_fat_entry 2 +exfat_fat_eoc)

		; Initialize root directory (empty directory with just end marker)
		(defq root_data (cat
			(char +exfat_entry_alloc_bitmap)
			(str-alloc (- +exfat_entry_size 1))
			(str-alloc (- cluster_size +exfat_entry_size))))
		(. this :write_cluster 2 root_data)

		(def this :mounted :t)
	 :t)

	(defmethod :write_boot_sector (volume_size)
		; Write the ExFat boot sector
		; inputs
		; volume_size = total volume size in bytes
		(defq sector_size (get this :sector_size)
			cluster_shift (get this :cluster_shift)
			fat_offset (get this :fat_offset)
			fat_length (get this :fat_length)
			cluster_heap_offset (get this :cluster_heap_offset)
			cluster_count (get this :cluster_count)
			root_cluster (get this :root_dir_cluster))

		; Build boot sector (512 bytes)
		(defq boot_sector (cat
			; Jump boot code (3 bytes)
			(char 0xEB) (char 0x76) (char 0x90)
			; File system name "EXFAT   " (8 bytes)
			"EXFAT   "
			; Zeros (53 bytes) - must be zero
			(str-alloc 53)
			; Partition offset (8 bytes) - always 0
			(u64-to-bytes-le 0)
			; Volume length (8 bytes) - total sectors
			(u64-to-bytes-le (/ volume_size sector_size))
			; FAT offset (4 bytes)
			(u32-to-bytes-le fat_offset)
			; FAT length (4 bytes)
			(u32-to-bytes-le fat_length)
			; Cluster heap offset (4 bytes)
			(u32-to-bytes-le cluster_heap_offset)
			; Cluster count (4 bytes)
			(u32-to-bytes-le cluster_count)
			; Root directory first cluster (4 bytes)
			(u32-to-bytes-le root_cluster)
			; Volume serial number (4 bytes)
			(u32-to-bytes-le 0x12345678)
			; File system revision (2 bytes) - version 1.0
			(u16-to-bytes-le 0x0100)
			; Volume flags (2 bytes)
			(u16-to-bytes-le 0x0000)
			; Bytes per sector shift (1 byte) - 9 for 512 bytes
			(char 9)
			; Sectors per cluster shift (1 byte)
			(char (- cluster_shift 9))
			; Number of FATs (1 byte) - always 1
			(char 1)
			; Drive select (1 byte)
			(char 0x80)
			; Percent in use (1 byte)
			(char 0)
			; Reserved (7 bytes)
			(str-alloc 7)
			; Boot code (390 bytes)
			(str-alloc 390)
			; Boot signature (2 bytes)
			(char 0x55) (char 0xAA)))

		; Write boot sector
		(. this :write_sector 0 boot_sector))

	(defmethod :mount ()
		; Mount the filesystem
		; outputs
		; :t if successful, :nil otherwise
		(when-bind (boot_sector (. this :read_sector 0))
			; Verify boot signature
			(when (and (= (code boot_sector 1 510) 0x55)
					(= (code boot_sector 1 511) 0xAA))
				; Read filesystem parameters
				(defq fat_offset (bytes-to-u32-le boot_sector 80)
					fat_length (bytes-to-u32-le boot_sector 84)
					cluster_heap_offset (bytes-to-u32-le boot_sector 88)
					cluster_count (bytes-to-u32-le boot_sector 92)
					root_cluster (bytes-to-u32-le boot_sector 96)
					sectors_per_cluster_shift (code boot_sector 1 109))

				(def this :fat_offset fat_offset
					:fat_length fat_length
					:cluster_heap_offset cluster_heap_offset
					:cluster_count cluster_count
					:root_dir_cluster root_cluster
					:cluster_shift (+ sectors_per_cluster_shift 9)
					:cluster_size (<< 1 (+ sectors_per_cluster_shift 9))
					:mounted :t)
			 :t)))

	(defmethod :unmount ()
		; Unmount the filesystem
		(stream-flush (get this :stream))
		(def this :mounted :nil)
	 :t)

	;;;;;;;;;;;;;;;;;;;;
	; File Operations (Simplified)
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :mkdir (path)
		; Create a directory
		; inputs
		; path = directory path (str)
		; outputs
		; :t if successful, :nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Allocate cluster for new directory
		(when-bind (new_cluster (. this :allocate_cluster))
			; Initialize empty directory
			(. this :write_cluster new_cluster (str-alloc (get this :cluster_size)))
			; TODO: Add entry to parent directory
		 :t))

	(defmethod :create (path)
		; Create a file
		; inputs
		; path = file path (str)
		; outputs
		; :t if successful, :nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Allocate cluster for file data
		(when-bind (new_cluster (. this :allocate_cluster))
			; Initialize empty file
			(. this :write_cluster new_cluster (str-alloc (get this :cluster_size)))
			; TODO: Add entry to parent directory
		 :t))

	(defmethod :open (path mode)
		; Open a file
		; inputs
		; path = file path (str)
		; mode = open mode (:read, :write, :append)
		; outputs
		; file handle (hash-map) or :nil 		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Create file handle
		(defq file_handle (hash :path path
			:mode mode
			:cluster 0
			:position 0
			:size 0))

		; Add to open files list
		(push (get this :open_files) file_handle)
		file_handle)

	(defmethod :close (file_handle)
		; Close a file
		; inputs
		; file_handle = file handle
		; outputs
		; :t if successful
		(defq open_files_list (get this :open_files))
		(def this :open_files (filter (lambda (h) (/= h file_handle)) open_files_list))
	 :t)

	(defmethod :read (file_handle buffer_unused size)
		; Read from a file
		; inputs
		; file_handle = file handle
		; buffer_unused = buffer (ignored, always allocates new)
		; size = bytes to read
		; outputs
		; str with data, or :nil on error
		(defq cluster_num (get file_handle :cluster)
			position (get file_handle :position))
		(when (and cluster_num (> cluster_num 0))
			(when-bind (cluster_data (. this :read_cluster cluster_num))
				(defq available (- (length cluster_data) position)
					to_read (min size available))
				(when (> to_read 0)
					(def file_handle :position (+ position to_read))
					(slice cluster_data position (+ position to_read))))))

	(defmethod :write (file_handle data)
		; Write to a file
		; inputs
		; file_handle = file handle
		; data = data to write (str)
		; outputs
		; bytes written, or -1 on error
		(defq cluster_num (get file_handle :cluster))
		(if (and cluster_num (> cluster_num 0))
			(progn
				(. this :write_cluster cluster_num data)
				(def file_handle :position (length data))
				(length data))
			-1))

	(defmethod :seek (file_handle offset whence)
		; Seek in a file
		; inputs
		; file_handle = file handle
		; offset = offset
		; whence = 0 (start), 1 (current), 2 (end)
		; outputs
		; new position
		(defq position (get file_handle :position)
			file_size (get file_handle :size))
		(defq new_pos (cond
			((= whence 0) offset)
			((= whence 1) (+ position offset))
			((= whence 2) (+ file_size offset))
			(t position)))
		(def file_handle :position (max 0 new_pos))
		new_pos)

	(defmethod :tell (file_handle)
		; Get current position
		(get file_handle :position))

	(defmethod :remove (path)
		; Remove a file
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement file removal
	 :t)

	(defmethod :rmdir (path)
		; Remove a directory
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement directory removal
	 :t)

	(defmethod :exists (path)
		; Check if path exists
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement existence check
	 :nil)

	(defmethod :stat (path)
		; Get file/directory info
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement stat
	 :nil)

	(defmethod :list (path)
		; List directory contents
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement directory listing
		(list))

	(defmethod :rename (old_path new_path)
		; Rename/move file or directory
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))
		; TODO: Implement rename
	 :t)

	(defmethod :get_size ()
		; Get filesystem size
		; outputs
		; total size in bytes
		(* (get this :cluster_count) (get this :cluster_size))))
