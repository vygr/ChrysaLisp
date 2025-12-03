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
			cluster_count (get this :cluster_count)
			cluster_heap_offset (get this :cluster_heap_offset)
			sector_size (get this :sector_size)
			cluster_shift (get this :cluster_shift)
			; Convert cluster number to sector number
			sectors_per_cluster (>> cluster_size (log2 sector_size))
			sector_num (+ cluster_heap_offset (* (- cluster_num 2) sectors_per_cluster)))
		; Validate cluster_num is in valid range [2, cluster_count+1]
		(when (and (>= cluster_num 2) (< cluster_num (+ cluster_count 2)))
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
			cluster_count (get this :cluster_count)
			cluster_heap_offset (get this :cluster_heap_offset)
			sector_size (get this :sector_size)
			sectors_per_cluster (>> cluster_size (log2 sector_size))
			sector_num (+ cluster_heap_offset (* (- cluster_num 2) sectors_per_cluster)))
		; Validate cluster_num is in valid range [2, cluster_count+1]
		(when (and (>= cluster_num 2) (< cluster_num (+ cluster_count 2)))
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
		(defq cluster_count (get this :cluster_count)
			fat_offset (get this :fat_offset)
			sector_size (get this :sector_size)
			; Each FAT entry is 4 bytes (32-bit)
			fat_byte_offset (* cluster_num 4)
			sector_num (+ fat_offset (/ fat_byte_offset sector_size))
			offset_in_sector (% fat_byte_offset sector_size))
		; Validate cluster_num is in valid range [0, cluster_count+1]
		(when (and (>= cluster_num 0) (< cluster_num (+ cluster_count 2)))
			(when-bind (sector_data (. this :read_sector sector_num))
				(bytes-to-u32-le sector_data offset_in_sector))))

	(defmethod :write_fat_entry (cluster_num value)
		; Write a FAT entry
		; inputs
		; cluster_num = cluster number
		; value = FAT entry value
		; outputs
		; :t if successful, :nil otherwise
		(defq cluster_count (get this :cluster_count)
			fat_offset (get this :fat_offset)
			sector_size (get this :sector_size)
			fat_byte_offset (* cluster_num 4)
			sector_num (+ fat_offset (/ fat_byte_offset sector_size))
			offset_in_sector (% fat_byte_offset sector_size))
		; Validate cluster_num is in valid range [0, cluster_count+1]
		(when (and (>= cluster_num 0) (< cluster_num (+ cluster_count 2)))
			(when-bind (sector_data (. this :read_sector sector_num))
				; Build new sector with updated FAT entry
				(defq new_entry (u32-to-bytes-le value)
					updated_sector (cat
						(slice sector_data 0 offset_in_sector)
						new_entry
						(slice sector_data (+ offset_in_sector 4) sector_size)))
				(. this :write_sector sector_num updated_sector))))

	(defmethod :allocate_cluster ()
		; Allocate a new cluster from the FAT
		; outputs
		; cluster number, or :nil if no free clusters
		(defq cluster_count (get this :cluster_count))
		; Start from cluster 2 (0 and 1 are reserved)
		; Iterate through cluster_count clusters: from 2 to (cluster_count + 1)
		(some! (lambda (cluster_idx)
				(when (= (. this :read_fat_entry cluster_idx) +exfat_fat_free)
					; Mark cluster as end of chain
					(. this :write_fat_entry cluster_idx +exfat_fat_eoc)
					cluster_idx))
			(+ cluster_count 2) :nil 2))

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

	(defmethod :free_cluster (cluster_num)
		; Free a single cluster (mark as free in FAT)
		; inputs
		; cluster_num = cluster number to free
		; outputs
		; :t if successful, :nil otherwise
		(when (and (>= cluster_num 2) (< cluster_num (+ (get this :cluster_count) 2)))
			(. this :write_fat_entry cluster_num +exfat_fat_free)
			:t))

	;;;;;;;;;;;;;;;;;;;;
	; Directory Entry Operations
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :read_dir_entries (cluster_num)
		; Read all directory entries from a cluster
		; inputs
		; cluster_num = cluster containing directory
		; outputs
		; list of directory entry hashes
		(defq entries (list))
		(when-bind (cluster_data (. this :read_cluster cluster_num))
			(defq offset 0
				cluster_size (get this :cluster_size))
			; Parse entries (each is 32 bytes)
			(while (< offset cluster_size)
				(defq entry_type (code cluster_data 1 offset))
				; Stop at end of directory
				(when (= entry_type +exfat_entry_eod)
					(return entries))
				; Create entry hash
				(defq entry (hash
					:type entry_type
					:offset offset
					:data (slice cluster_data offset (+ offset +exfat_entry_size))))
				(push entries entry)
				(setq offset (+ offset +exfat_entry_size)))
			entries))

	(defmethod :write_dir_entry (cluster_num entry_offset entry_data)
		; Write a directory entry to a cluster
		; inputs
		; cluster_num = cluster containing directory
		; entry_offset = byte offset in cluster (multiple of 32)
		; entry_data = entry data (32 bytes)
		; outputs
		; :t if successful
		(when-bind (cluster_data (. this :read_cluster cluster_num))
			(defq new_data (cat
				(slice cluster_data 0 entry_offset)
				entry_data
				(slice cluster_data (+ entry_offset +exfat_entry_size) -1)))
			(. this :write_cluster cluster_num new_data)))

	(defmethod :create_file_entry (name attributes)
		; Create a file directory entry set
		; inputs
		; name = file name (str)
		; attributes = file attributes (bitmap)
		; outputs
		; list of entry data strings (file + stream + name entries)
		(defq entries (list)
			name_len (length name)
			; Calculate number of name entries needed (15 chars per entry)
			name_entries (/ (+ name_len 14) 15))

		; File entry (primary)
		(defq file_entry (cat
			(char +exfat_entry_file)
			(char (+ 1 name_entries))  ; Secondary count
			(u16-to-bytes-le 0)  ; Checksum (TODO: calculate)
			(u16-to-bytes-le attributes)
			(str-alloc 2)  ; Reserved
			(u32-to-bytes-le 0)  ; Create timestamp
			(u32-to-bytes-le 0)  ; Modified timestamp
			(u32-to-bytes-le 0)  ; Accessed timestamp
			(char 0) (char 0)  ; Create 10ms, timezone
			(char 0) (char 0)  ; Modified 10ms, timezone
			(char 0) (char 0)  ; Accessed 10ms, timezone
			(str-alloc 7)))  ; Reserved
		(push entries file_entry)

		; Stream extension entry
		(defq stream_entry (cat
			(char +exfat_entry_stream_ext)
			(char 0)  ; Flags
			(char 0)  ; Reserved
			(char name_len)  ; Name length
			(u16-to-bytes-le 0)  ; Name hash
			(str-alloc 2)  ; Reserved
			(u64-to-bytes-le 0)  ; Valid data length
			(str-alloc 4)  ; Reserved
			(u32-to-bytes-le 0)  ; First cluster
			(u64-to-bytes-le 0)))  ; Data length
		(push entries stream_entry)

		; File name entries
		(defq name_offset 0)
		(each! (lambda (i)
			(defq chunk_start name_offset
				chunk_end (min (+ name_offset 15) name_len)
				chunk (if (< chunk_start name_len)
					(slice name chunk_start chunk_end)
					"")
				; Pad to 30 bytes (15 UTF-16 chars)
				padded (cat chunk (str-alloc (- 30 (* (length chunk) 2)))))
			(defq name_entry (cat
				(char +exfat_entry_file_name)
				(char 0)  ; Flags
				padded))
			(push entries name_entry)
			(setq name_offset chunk_end))
			name_entries 0)

		entries)

	(defmethod :parse_file_entry (entries entry_idx)
		; Parse a file entry set starting at entry_idx
		; inputs
		; entries = list of all directory entries
		; entry_idx = index of file entry
		; outputs
		; hash with file info, or :nil
		(when (< entry_idx (length entries))
			(defq file_entry (get entries entry_idx)
				file_data (get file_entry :data))

			; Verify it's a file entry
			(when (= (get file_entry :type) +exfat_entry_file)
				(defq secondary_count (code file_data 1 1)
					attributes (bytes-to-u16-le file_data 4))

				; Get stream extension entry
				(when (< (+ entry_idx 1) (length entries))
					(defq stream_entry (get entries (+ entry_idx 1))
						stream_data (get stream_entry :data))

					(when (= (get stream_entry :type) +exfat_entry_stream_ext)
						(defq name_len (code stream_data 1 3)
							first_cluster (bytes-to-u32-le stream_data 20)
							data_length (bytes-to-u32-le stream_data 24))  ; Use 32-bit portion

						; Parse file name from name entries
						(defq name ""
							name_entry_count (- secondary_count 1)
							i 0)
						(while (< i name_entry_count)
							(when (< (+ entry_idx 2 i) (length entries))
								(defq name_entry (get entries (+ entry_idx 2 i))
									name_data (get name_entry :data))
								(when (= (get name_entry :type) +exfat_entry_file_name)
									; Extract name chunk (skip type and flags bytes)
									(defq chunk_data (slice name_data 2 32)
										chunk_len (min 15 (- name_len (length name))))
									; For simplicity, extract ASCII (ignoring UTF-16)
									(defq j 0)
									(while (< j chunk_len)
										(setq name (cat name (char (code chunk_data 1 (* j 2)))))
										(setq j (inc j)))))
							(setq i (inc i)))

						; Return file info hash
						(hash
							:name name
							:attributes attributes
							:first_cluster first_cluster
							:size data_length
							:is_directory (= (logand attributes +exfat_attr_directory) +exfat_attr_directory)))))))

	(defmethod :find_free_entry_space (dir_cluster entry_count)
		; Find free space in directory for entry_count entries
		; inputs
		; dir_cluster = directory cluster number
		; entry_count = number of consecutive entries needed
		; outputs
		; byte offset of first free entry, or :nil if no space
		(when-bind (cluster_data (. this :read_cluster dir_cluster))
			(defq offset 0
				cluster_size (get this :cluster_size)
				consecutive_free 0
				start_offset :nil)

			(while (< offset cluster_size)
				(defq entry_type (code cluster_data 1 offset))

				; Check if entry is free (0x00 or 0xE5)
				(if (or (= entry_type +exfat_entry_eod) (= entry_type 0x00) (= entry_type 0xE5))
					(progn
						; Mark start of free space
						(unless start_offset
							(setq start_offset offset))
						(setq consecutive_free (inc consecutive_free))

						; Found enough space?
						(when (>= consecutive_free entry_count)
							(return start_offset)))
					; Reset if we hit a used entry
					(progn
						(setq consecutive_free 0)
						(setq start_offset :nil)))

				(setq offset (+ offset +exfat_entry_size)))

			:nil))

	(defmethod :add_entry_to_directory (dir_cluster name attributes first_cluster file_size)
		; Add a file/directory entry to a parent directory
		; inputs
		; dir_cluster = parent directory cluster
		; name = file/directory name
		; attributes = file attributes
		; first_cluster = first cluster of file/directory
		; file_size = size of file (0 for directories)
		; outputs
		; :t if successful, :nil otherwise
		(defq entry_list (. this :create_file_entry name attributes)
			entry_count (length entry_list))

		; Find free space in directory
		(when-bind (free_offset (. this :find_free_entry_space dir_cluster entry_count))
			; Update stream extension with actual cluster and size
			(when (>= (length entry_list) 2)
				(defq stream_entry (get entry_list 1)
					; Rebuild stream entry with correct cluster and size
					new_stream (cat
						(slice stream_entry 0 20)  ; Keep first 20 bytes
						(u32-to-bytes-le first_cluster)  ; First cluster at offset 20
						(u64-to-bytes-le file_size)))  ; Data length at offset 24

				; Update entry list
				(setq entry_list (cat
					(list (get entry_list 0))
					(list new_stream)
					(slice entry_list 2 -1))))

			; Write all entries
			(defq success :t
				i 0)
			(while (and success (< i entry_count))
				(defq entry_data (get entry_list i)
					offset (+ free_offset (* i +exfat_entry_size)))
				(unless (. this :write_dir_entry dir_cluster offset entry_data)
					(setq success :nil))
				(setq i (inc i)))

			success))

	(defmethod :remove_entry_from_directory (dir_cluster name)
		; Remove an entry from a directory
		; inputs
		; dir_cluster = directory cluster containing the entry
		; name = name of file/directory to remove
		; outputs
		; :t if successful, :nil if not found
		(when-bind (entries (. this :read_dir_entries dir_cluster))
			(defq i 0)
			(while (< i (length entries))
				(defq entry (get entries i))
				(when (= (get entry :type) +exfat_entry_file)
					(when-bind (file_info (. this :parse_file_entry entries i))
						(when (= (get file_info :name) name)
							; Found the entry - mark all its entries as deleted
							(defq file_entry (get entries i)
								file_data (get file_entry :data)
								secondary_count (code file_data 1 1)
								total_entries (+ 1 secondary_count))

							; Mark each entry as deleted (0xE5)
							(defq j 0
								success :t)
							(while (and success (< j total_entries))
								(defq del_offset (get (get entries (+ i j)) :offset)
									del_entry (cat (char 0xE5) (str-alloc (- +exfat_entry_size 1))))
								(unless (. this :write_dir_entry dir_cluster del_offset del_entry)
									(setq success :nil))
								(setq j (inc j)))

							(return success))))
				(setq i (inc i)))
			:nil))

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
	; Path Resolution
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :split_path (path)
		; Split path into components
		; inputs
		; path = file path (str) like "/dir/file.txt"
		; outputs
		; list of path components
		(defq components (list)
			current ""
			i 0
			path_len (length path))
		; Skip leading slash
		(when (and (> path_len 0) (= (code path 1 0) 47))  ; 47 = '/'
			(setq i 1))
		(while (< i path_len)
			(defq ch (code path 1 i))
			(if (= ch 47)  ; '/'
				(when (> (length current) 0)
					(push components current)
					(setq current ""))
				(setq current (cat current (char ch))))
			(setq i (inc i)))
		; Add final component
		(when (> (length current) 0)
			(push components current))
		components)

	(defmethod :find_in_directory (dir_cluster name)
		; Find a file/directory entry by name in a directory
		; inputs
		; dir_cluster = directory cluster number
		; name = file/directory name to find
		; outputs
		; file info hash or :nil if not found
		(when-bind (entries (. this :read_dir_entries dir_cluster))
			(defq i 0)
			(while (< i (length entries))
				(defq entry (get entries i))
				(when (= (get entry :type) +exfat_entry_file)
					(when-bind (file_info (. this :parse_file_entry entries i))
						(when (= (get file_info :name) name)
							(return file_info))))
				(setq i (inc i)))
			:nil))

	(defmethod :resolve_path (path)
		; Resolve a path to a file info hash
		; inputs
		; path = file path (str)
		; outputs
		; file info hash or :nil if not found
		(unless (get this :mounted)
			(return :nil))

		; Start at root directory
		(defq current_cluster (get this :root_dir_cluster)
			components (. this :split_path path))

		; Handle root directory
		(when (= (length components) 0)
			(return (hash
				:name "/"
				:attributes +exfat_attr_directory
				:first_cluster current_cluster
				:size 0
				:is_directory :t)))

		; Navigate path
		(defq i 0
			found :nil)
		(while (< i (length components))
			(defq component (get components i))
			(setq found (. this :find_in_directory current_cluster component))
			(unless found
				(return :nil))
			; If not last component, must be a directory
			(when (< (+ i 1) (length components))
				(unless (get found :is_directory)
					(return :nil))
				(setq current_cluster (get found :first_cluster)))
			(setq i (inc i)))

		found)

	;;;;;;;;;;;;;;;;;;;;
	; File Operations
	;;;;;;;;;;;;;;;;;;;;

	(defmethod :mkdir (path)
		; Create a directory
		; inputs
		; path = directory path (str)
		; outputs
		; :t if successful, :nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Check if path already exists
		(when (. this :resolve_path path)
			(return :nil))

		; Get parent directory and name
		(defq components (. this :split_path path))
		(when (= (length components) 0)
			(return :nil))

		(defq name (get components -2)
			parent_components (slice components 0 -2)
			parent_path (if (= (length parent_components) 0)
				"/"
				(cat "/" (join parent_components "/"))))

		; Find parent directory
		(defq parent_info (. this :resolve_path parent_path))
		(unless (and parent_info (get parent_info :is_directory))
			(return :nil))

		; Allocate cluster for new directory
		(when-bind (new_cluster (. this :allocate_cluster))
			; Initialize empty directory with end-of-directory marker
			(defq dir_data (cat (char +exfat_entry_eod) (str-alloc (- (get this :cluster_size) 1))))
			(. this :write_cluster new_cluster dir_data)

			; Add directory entry to parent
			(defq parent_cluster (get parent_info :first_cluster))
			(. this :add_entry_to_directory parent_cluster name +exfat_attr_directory new_cluster 0))

	(defmethod :create (path)
		; Create a file
		; inputs
		; path = file path (str)
		; outputs
		; :t if successful, :nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Check if path already exists
		(when (. this :resolve_path path)
			(return :nil))

		; Get parent directory and name
		(defq components (. this :split_path path))
		(when (= (length components) 0)
			(return :nil))

		(defq name (get components -2)
			parent_components (slice components 0 -2)
			parent_path (if (= (length parent_components) 0)
				"/"
				(cat "/" (join parent_components "/"))))

		; Find parent directory
		(defq parent_info (. this :resolve_path parent_path))
		(unless (and parent_info (get parent_info :is_directory))
			(return :nil))

		; Allocate cluster for file data
		(when-bind (new_cluster (. this :allocate_cluster))
			; Initialize empty file
			(. this :write_cluster new_cluster (str-alloc (get this :cluster_size)))

			; Add file entry to parent
			(defq parent_cluster (get parent_info :first_cluster))
			(. this :add_entry_to_directory parent_cluster name +exfat_attr_archive new_cluster 0)))

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
		; inputs
		; path = file path (str)
		; outputs
		; :t if successful, :nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Find the file
		(when-bind (file_info (. this :resolve_path path))
			; Can't remove directories with remove
			(when (get file_info :is_directory)
				(return :nil))

			; Get parent directory
			(defq components (. this :split_path path)
				name (get components -2)
				parent_components (slice components 0 -2)
				parent_path (if (= (length parent_components) 0)
					"/"
					(cat "/" (join parent_components "/"))))

			(when-bind (parent_info (. this :resolve_path parent_path))
				; Remove entry from parent directory
				(defq parent_cluster (get parent_info :first_cluster))
				(. this :remove_entry_from_directory parent_cluster name)

				; Free the cluster chain
				(defq first_cluster (get file_info :first_cluster))
				(when (and first_cluster (>= first_cluster 2))
					(. this :free_cluster_chain first_cluster))

				:t)))

	(defmethod :rmdir (path)
		; Remove a directory
		; inputs
		; path = directory path (str)
		; outputs
		; :t if successful, :nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Find the directory
		(when-bind (dir_info (. this :resolve_path path))
			; Must be a directory
			(unless (get dir_info :is_directory)
				(return :nil))

			; Check if directory is empty
			(defq dir_cluster (get dir_info :first_cluster))
			(when-bind (entries (. this :read_dir_entries dir_cluster))
				; Directory should only have end-of-directory marker if empty
				(when (> (length entries) 0)
					(return :nil)))

			; Get parent directory
			(defq components (. this :split_path path)
				name (get components -2)
				parent_components (slice components 0 -2)
				parent_path (if (= (length parent_components) 0)
					"/"
					(cat "/" (join parent_components "/"))))

			(when-bind (parent_info (. this :resolve_path parent_path))
				; Remove entry from parent directory
				(defq parent_cluster (get parent_info :first_cluster))
				(. this :remove_entry_from_directory parent_cluster name)

				; Free the directory cluster
				(when (and dir_cluster (>= dir_cluster 2))
					(. this :free_cluster dir_cluster))

				:t)))

	(defmethod :exists (path)
		; Check if path exists
		; inputs
		; path = file path (str)
		; outputs
		; :t if exists, :nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		(if (. this :resolve_path path) :t :nil))

	(defmethod :stat (path)
		; Get file/directory info
		; inputs
		; path = file path (str)
		; outputs
		; file info hash or :nil
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		(. this :resolve_path path))

	(defmethod :list (path)
		; List directory contents
		; inputs
		; path = directory path (str)
		; outputs
		; list of file info hashes
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Resolve directory path
		(defq dir_info (. this :resolve_path path))
		(unless (and dir_info (get dir_info :is_directory))
			(return (list)))

		; Read directory entries
		(defq dir_cluster (get dir_info :first_cluster)
			files (list))

		(when-bind (entries (. this :read_dir_entries dir_cluster))
			(defq i 0)
			(while (< i (length entries))
				(defq entry (get entries i))
				(when (= (get entry :type) +exfat_entry_file)
					(when-bind (file_info (. this :parse_file_entry entries i))
						(push files file_info)))
				(setq i (inc i))))

		files)

	(defmethod :rename (old_path new_path)
		; Rename/move file or directory
		; inputs
		; old_path = current path (str)
		; new_path = new path (str)
		; outputs
		; :t if successful, :nil otherwise
		(unless (get this :mounted)
			(throw "Filesystem not mounted" this))

		; Check old path exists
		(when-bind (old_info (. this :resolve_path old_path))
			; Check new path doesn't exist
			(when (. this :resolve_path new_path)
				(return :nil))

			; Get old parent and name
			(defq old_components (. this :split_path old_path)
				old_name (get old_components (- (length old_components) 1))
				old_parent_components (slice old_components 0 (- (length old_components) 1))
				old_parent_path (if (= (length old_parent_components) 0)
					"/"
					(cat "/" (join old_parent_components "/"))))

			; Get new parent and name
			(defq new_components (. this :split_path new_path)
				new_name (get new_components (- (length new_components) 1))
				new_parent_components (slice new_components 0 (- (length new_components) 1))
				new_parent_path (if (= (length new_parent_components) 0)
					"/"
					(cat "/" (join new_parent_components "/"))))

			; Verify new parent exists
			(when-bind (new_parent_info (. this :resolve_path new_parent_path))
				(when-bind (old_parent_info (. this :resolve_path old_parent_path))
					; Remove from old parent
					(defq old_parent_cluster (get old_parent_info :first_cluster))
					(. this :remove_entry_from_directory old_parent_cluster old_name)

					; Add to new parent
					(defq new_parent_cluster (get new_parent_info :first_cluster)
						first_cluster (get old_info :first_cluster)
						size (get old_info :size)
						attributes (get old_info :attributes))

					(. this :add_entry_to_directory new_parent_cluster new_name attributes first_cluster size)))))

	(defmethod :get_size ()
		; Get filesystem size
		; outputs
		; total size in bytes
		(defq size (* (get this :cluster_count) (get this :cluster_size)))
		size))) ; End of ExFat class
