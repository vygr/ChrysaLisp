(import "./fs.inc")
(import "lib/collections/fmap.inc")

;; ExFAT Constants
(defq +EXFAT_SECTOR_SIZE 512)
(defq +EXFAT_SECTOR_BITS 9)
(defq +EXFAT_CLUSTER_SIZE 4096) ; 4KB default
(defq +EXFAT_CLUSTER_BITS 12)
(defq +EXFAT_ROOT_DIR_CLUSTER 2) ; Usually starts at cluster 2

;; Entry Types
(defq +EXFAT_TYPE_EOD	   0x00)
(defq +EXFAT_TYPE_BITMAP	0x81)
(defq +EXFAT_TYPE_UPCASE	0x82)
(defq +EXFAT_TYPE_LABEL	 0x83)
(defq +EXFAT_TYPE_FILE	  0x85)
(defq +EXFAT_TYPE_STREAM	0xC0)
(defq +EXFAT_TYPE_NAME	  0xC1)

;; File Attributes
(defq +EXFAT_ATTR_READONLY  0x01)
(defq +EXFAT_ATTR_HIDDEN	0x02)
(defq +EXFAT_ATTR_SYSTEM	0x04)
(defq +EXFAT_ATTR_VOLUME	0x08)
(defq +EXFAT_ATTR_DIR	   0x10)
(defq +EXFAT_ATTR_ARCHIVE   0x20)

;; Boot Sector Structure
(structure +exfat_boot 0
	(struct jump_boot 3)
	(struct fs_name 8)
	(struct zero 53)
	(ulong partition_offset)
	(ulong volume_length)
	(uint fat_offset)
	(uint fat_length)
	(uint heap_offset)
	(uint cluster_count)
	(uint root_cluster)
	(uint serial_number)
	(ushort fs_revision)
	(ushort volume_flags)
	(ubyte bytes_per_sector_shift)
	(ubyte sectors_per_cluster_shift)
	(ubyte num_fats)
	(ubyte drive_select)
	(ubyte percent_in_use)
	(struct reserved 7)
	(struct code 390)
	(ushort signature)
)

;; Base Entry Header - all directory entries inherit from this
(structure +exfat_entry_header 0
	(ubyte type)
)

;; Generic Directory Entry
(structure +exfat_entry +exfat_entry_header_size
	(struct custom 19)
	(uint first_cluster)
	(ulong data_length)
)

;; Allocation Bitmap Entry
(structure +exfat_bitmap +exfat_entry_header_size
	(ubyte flags)
	(struct reserved 18)
	(uint first_cluster)
	(ulong data_length)
)

;; Up-case Table Entry
(structure +exfat_upcase +exfat_entry_header_size
	(struct reserved1 3)
	(uint checksum)
	(struct reserved2 12)
	(uint first_cluster)
	(ulong data_length)
)

;; Volume Label Entry
(structure +exfat_label +exfat_entry_header_size
	(ubyte char_count)
	(struct label 22) ; 11 chars * 2 bytes
	(struct reserved 8)
)

;; File Directory Entry
(structure +exfat_file +exfat_entry_header_size
	(ubyte secondary_count)
	(ushort checksum)
	(ushort attr)
	(ushort reserved1)
	(uint create_timestamp)
	(uint modify_timestamp)
	(uint access_timestamp)
	(ubyte create_10ms)
	(ubyte modify_10ms)
	(ubyte create_tz)
	(ubyte modify_tz)
	(ubyte access_tz)
	(struct reserved2 7)
)

;; Stream Extension Entry
(structure +exfat_stream +exfat_entry_header_size
	(ubyte flags)
	(ubyte reserved1)
	(ubyte name_len)
	(ushort name_hash)
	(ushort reserved2)
	(ulong valid_len)
	(uint reserved3)
	(uint first_cluster)
	(ulong data_len)
)

;; File Name Entry
(structure +exfat_name +exfat_entry_header_size
	(ubyte flags)
	(struct name 30) ; 15 chars * 2 bytes
)

;; FAT Entry
(structure +fat_entry 0
	(uint entry)
)

;; Helper Structure for FAT entries
(structure +fat_entries 0
	(uint entry0)
	(uint entry1)
	(uint entry2)
)

;; Helper Structure for FAT reserved entries (clusters 0 and 1)
(structure +fat_reserved 0
	(uint entry0)
	(uint entry1)
)

;; Helper for bitmap data
(structure +exfat_bitmap_data 0 (ubyte val))

;; Helper for upcase data (used with obj-set in loops)
(structure +exfat_upcase_data 0 (ushort val))

(defclass ExFat () (Fs)
	;; Initialize handle management
	(def this :handles (Fmap) :next_handle 1)
	
	(defmethod :mount ()
		(raise :stream)
		(unless stream (throw "No stream to mount" this))
		
		;; Read Boot Sector
		(stream-seek stream 0 0)
		(defq boot (read-blk stream +exfat_boot_size))
		
		;; Validate Signature
		(if (/= (getf boot +exfat_boot_signature) 0xAA55)
			(throw "Invalid ExFAT signature" this))
			
		;; Validate FS Name
		(if (not (eql (get-str boot +exfat_boot_fs_name 5) "EXFAT"))
			(throw "Invalid ExFAT FS Name" this))
			
		;; Load Geometry
		(lower
			(:sector_size (defq sector_size (<< 1 (getf boot +exfat_boot_bytes_per_sector_shift))))
			(:cluster_size (<< sector_size (getf boot +exfat_boot_sectors_per_cluster_shift)))
			(:fat_offset (getf boot +exfat_boot_fat_offset))
			(:fat_length (getf boot +exfat_boot_fat_length))
			(:heap_offset (getf boot +exfat_boot_heap_offset))
			(:cluster_count (getf boot +exfat_boot_cluster_count))
			(:root_cluster (getf boot +exfat_boot_root_cluster))
			(:total_sectors (getf boot +exfat_boot_volume_length))
		)
		(. this :find_bitmap)
		this
	)

	(defmethod :unmount ()
		(raise :stream)
		(if stream
			(stream-flush stream))
		this
	)

	(defmethod :find_bitmap ()
		(raise :stream :root_cluster :cluster_size)
		(defq current_cluster root_cluster)
		(defq found_cluster 0)
		(while (and (< current_cluster 0xFFFFFFF8) (= found_cluster 0))
			(defq data (. this :read_cluster current_cluster))
			(defq idx 0)
			(while (and (< idx cluster_size) (= found_cluster 0))
				(defq type (get-ubyte data idx))
				(if (= type +EXFAT_TYPE_BITMAP)
					(progn
						(setq found_cluster (get-uint data (+ idx 20)))
						(defq bitmap_len (get-long data (+ idx 24)))
						(lower (:bitmap_cluster found_cluster))
					)
				)
				(setq idx (+ idx 32))
			)
			(setq current_cluster (. this :get_fat_entry current_cluster))
		)
		(if (= found_cluster 0) (throw "Bitmap not found" this))
		found_cluster
	)

	(defmethod :find_bitmap ()
		(raise :stream :root_cluster :cluster_size)
		(defq current_cluster root_cluster)
		(defq found_cluster 0)
		(while (and (< current_cluster 0xFFFFFFF8) (= found_cluster 0))
			(defq data (. this :read_cluster current_cluster))
			(defq idx 0)
			(while (and (< idx cluster_size) (= found_cluster 0))
				(defq type (get-ubyte data idx))
				(if (= type +EXFAT_TYPE_BITMAP)
					(progn
						(setq found_cluster (get-uint data (+ idx 20)))
						(defq bitmap_len (get-long data (+ idx 24)))
						(lower (:bitmap_cluster found_cluster))
					)
				)
				(setq idx (+ idx 32))
			)
			(setq current_cluster (. this :get_fat_entry current_cluster))
		)
		(if (= found_cluster 0) (throw "Bitmap not found" this))
		found_cluster
	)

	;;Directory Operations
	(defmethod :read_dir_cluster (cluster)
		(raise :stream :cluster_size)
		(defq data (. this :read_cluster cluster))
		(defq entries (list))
		(defq idx 0)
		(while (< idx cluster_size)
			(defq type (get-ubyte data idx))
			(when (= type +EXFAT_TYPE_FILE)
				(defq stream_offset (+ idx 32))
				(defq name_offset (+ idx 64))
				(when (< name_offset cluster_size)
					(defq name_len (get-ubyte data (+ stream_offset 3)))
					(defq utf16_data (get-str data (+ name_offset 2) (* name_len 2)))
					;; Convert UTF-16LE to ASCII
					(defq ascii_name (str-alloc name_len))
					(defq i 0)
					(while (< i name_len)
						(set-byte ascii_name i (get-ushort utf16_data (* i 2)))
						(setq i (+ i 1))
					)
					(defq first_cluster (get-uint data (+ stream_offset 20)))
					(defq data_len (get-long data (+ stream_offset 24)))
					(defq attr (get-ushort data (+ idx 4)))
					(push entries (list ascii_name first_cluster data_len attr))
				)
			)
			(setq idx (+ idx 32))
		)
		entries
	)

	(defmethod :list_dir (dir_cluster)
		(raise :stream)
		(defq current_cluster dir_cluster)
		(defq all_entries (list))
		(while (< current_cluster 0xFFFFFFF8)
			(defq cluster_entries (. this :read_dir_cluster current_cluster))
			(each! (lambda (e) (push all_entries e)) (list cluster_entries))
			(setq current_cluster (. this :get_fat_entry current_cluster))
		)
		all_entries
	)

	(defmethod :find_entry (dir_cluster name)
		(raise :stream :cluster_size)
		(defq current_cluster dir_cluster)
		(defq found_info :nil)
		
		(while (and (< current_cluster 0xFFFFFFF8) (ifn found_info :t))
			(defq data (. this :read_cluster current_cluster))
			(defq idx 0)
			(while (and (< idx cluster_size) (ifn found_info :t))
				(defq type (get-ubyte data idx))
				
				(when (= type +EXFAT_TYPE_FILE)
					(defq stream_offset (+ idx 32))
					(defq name_offset (+ idx 64))
					
					(when (< name_offset cluster_size)
						(defq name_len (get-ubyte data (+ stream_offset 3)))
						(defq entry_name (get-str data (+ name_offset 2) (* name_len 2)))
						
						(defq match :t)
						(defq i 0)
						(if (/= name_len (length name))
							(setq match :nil)
							(while (< i name_len)
								(defq char_code (get-ushort entry_name (* i 2)))
								(defq name_char (code name 1 i))
								(when (/= char_code name_char)
									(setq match :nil))
								(setq i (+ i 1))
							)
						)
						
						(when match
							(defq first_cluster (get-uint data (+ stream_offset 20)))
							(defq data_len (get-long data (+ stream_offset 24)))
							(defq attr (get-ushort data (+ idx 4)))
							(setq found_info (list first_cluster data_len attr))
						)
					)
				)
				(setq idx (+ idx 32))
			)
			(setq current_cluster (. this :get_fat_entry current_cluster))
		)
		found_info
	)

	(defmethod :resolve_path (path)
		(raise :root_cluster)
		(defq current_cluster root_cluster)
		
		(defq start 0)
		(defq end 0)
		(defq len (length path))
		(defq info :nil)
		
		;; Special case for root directory
		(if (and (= len 1) (eql path "/"))
			(setq info (list root_cluster 0 +EXFAT_ATTR_DIR))
			(progn
				(when (eql (slice path 0 1) "/") (setq start 1))
				
				(while (< start len)
					(setq end (find "/" path start))
					(ifn end (setq end len))
					
					(defq part (slice path start end))
					(when (nempty? part)
						(setq info (. this :find_entry current_cluster part))
						(when info
							(setq current_cluster (first info)))
						(unless info
							(setq start len)))  ;; Exit loop if not found
					(setq start (+ end 1)))))
		info
	)

	(defmethod :create_entry (dir_cluster name attr first_cluster data_len)
		(raise :stream :cluster_size)
		;; Need 3 contiguous entries: File, Stream, Name
		;; Simplification: Scan for EOD (0x00) or enough deleted entries.
		;; For now, just append to end (EOD).
		
		(defq current_cluster dir_cluster)
		(defq found_idx -1)
		(defq found_cluster 0)
		
		(while (and (< current_cluster 0xFFFFFFF8) (= found_idx -1))
			(defq data (. this :read_cluster current_cluster))
			(defq idx 0)
			(while (and (< idx cluster_size) (= found_idx -1))
				(defq type (get-ubyte data idx))
				(when (= type 0) ;; EOD
					;; Check if we have space for 3 entries (96 bytes)
					(if (<= (+ idx 96) cluster_size)
						(progn
							(setq found_idx idx)
							(setq found_cluster current_cluster)
						)
						;; Not enough space in this cluster, need to extend chain or go to next
						;; For now, assume we can extend or jump to next if EOD is at end
					)
				)
				(setq idx (+ idx 32))
			)
			(if (= found_idx -1)
				(progn
					(defq next_cluster (. this :get_fat_entry current_cluster))
					(if (>= next_cluster 0xFFFFFFF8)
						(progn
							;; Allocate new cluster
							(defq new_cluster (. this :alloc_cluster))
							;; Zero it out
							(. this :write_cluster new_cluster (pad "" cluster_size (ascii-char 0)))
							;; Link
							(. this :set_fat_entry current_cluster new_cluster)
							(. this :set_fat_entry new_cluster 0xFFFFFFFF)
							(setq current_cluster new_cluster)
						)
						(setq current_cluster next_cluster)
					)
				)
			)
		)
		
		
		;; Write Entries
		(stream-seek stream (. this :cluster_to_offset found_cluster) 0)
		(defq cluster_data (read-blk stream cluster_size))
		
		;; File Entry
		(set-byte cluster_data found_idx +EXFAT_TYPE_FILE)
		(set-byte cluster_data (+ found_idx 1) 2) ;; secondary_count
		(set-short cluster_data (+ found_idx 4) attr)
		
		;; Stream Entry
		(defq stream_idx (+ found_idx 32))
		(set-byte cluster_data stream_idx +EXFAT_TYPE_STREAM)
		(set-byte cluster_data (+ stream_idx 1) (if (= attr +EXFAT_ATTR_DIR) 1 3))
		(set-byte cluster_data (+ stream_idx 3) (length name))
		(set-int cluster_data (+ stream_idx 20) first_cluster)
		(set-long cluster_data (+ stream_idx 24) data_len)
		(set-long cluster_data (+ stream_idx 8) data_len) ;; valid_len
		
		;; Name Entry
		(defq name_idx (+ found_idx 64))
		(set-byte cluster_data name_idx +EXFAT_TYPE_NAME)
		(set-byte cluster_data (+ name_idx 1) 0) ;; flags
		;; Write Name (UTF-16LE)
		(defq i 0)
		(while (< i (length name))
			(set-short cluster_data (+ found_idx 66 (* i 2)) (code name 1 i))
			(setq i (+ i 1))
		)
		
		(stream-seek stream (. this :cluster_to_offset found_cluster) 0)
		(write-blk stream cluster_data)
		this
	)

	(defmethod :create_sub_dir (parent_cluster name)
		(raise :cluster_size)
		(defq new_cluster (. this :alloc_cluster))
		;; Zero out new cluster
		(. this :write_cluster new_cluster (pad "" cluster_size (ascii-char 0)))
		(. this :set_fat_entry new_cluster 0xFFFFFFFF)
		
		(. this :create_entry parent_cluster name +EXFAT_ATTR_DIR new_cluster 0)
		new_cluster
	)

	(defmethod :create_dir (path)
		;; Create a directory at the given path
		(raise :root_cluster)
		
		;; Parse path to get parent and filename
		(when (defq last_slash (rfind "/" path))
			;; Extract parent path and filename
			(defq parent_path (slice path 0 last_slash))
			(defq dirname (slice path last_slash -1))
			
			;; Resolve parent directory
			(defq parent_cluster (if (or (= (length parent_path) 0) (eql parent_path "/"))
				root_cluster
				(first (. this :resolve_path parent_path))))
			
			(. this :create_sub_dir parent_cluster dirname))
	)
	
	(defmethod :free_chain (start_cluster)
		(defq current_cluster start_cluster)
		(while (and (>= current_cluster 2) (< current_cluster 0xFFFFFFF8))
			(defq next_cluster (. this :get_fat_entry current_cluster))
			(. this :free_cluster current_cluster)
			(. this :set_fat_entry current_cluster 0) ;; Clear FAT entry
			(setq current_cluster next_cluster)
		)
		this
	)

	(defmethod :delete_entry (dir_cluster name)
		(raise :stream :cluster_size)
		(defq current_cluster dir_cluster)
		(defq found :nil)
		
		(while (and (< current_cluster 0xFFFFFFF8) (ifn found :t))
			(defq data (. this :read_cluster current_cluster))
			(defq idx 0)
			(while (and (< idx cluster_size) (ifn found :t))
				(defq type (get-ubyte data idx))
				
				(when (= type +EXFAT_TYPE_FILE)
					(defq stream_offset (+ idx 32))
					(defq name_offset (+ idx 64))
					
					(when (< name_offset cluster_size)
						(defq name_len (get-ubyte data (+ stream_offset 3)))
						(defq entry_name (get-str data (+ name_offset 2) (* name_len 2)))
						
						(defq match :t)
						(defq i 0)
						(if (/= name_len (length name))
							(setq match :nil)
							(while (< i name_len)
								(defq char_code (get-ushort entry_name (* i 2)))
								(defq name_char (code name 1 i))
								(when (/= char_code name_char)
									(setq match :nil))
								(setq i (+ i 1))
							)
						)
						
						(when match
							(setq found :t)
							;; Free the file's cluster chain
							(defq first_cluster (get-uint data (+ stream_offset 20)))
							(if (>= first_cluster 2)
								(. this :free_chain first_cluster))
							
							;; Mark entries as deleted (File, Stream, Name)
							;; Clear the "In Use" bit (0x80)
							(set-byte data idx (logand type 0x7F)) ;; File
							(set-byte data stream_offset (logand (get-ubyte data stream_offset) 0x7F)) ;; Stream
							(set-byte data name_offset (logand (get-ubyte data name_offset) 0x7F)) ;; Name
							
							;; Write back the cluster
							(stream-seek stream (. this :cluster_to_offset current_cluster) 0)
							(write-blk stream data)
						)
					)
				)
				(setq idx (+ idx 32))
			)
			(setq current_cluster (. this :get_fat_entry current_cluster))
		)
		;; Return nil if not found
		(unless found (setq this :nil))
		this
	)

	(defmethod :stat (path)
		;; Gets file/directory metadata
		;; Returns list: (name size is_dir attr first_cluster) or :nil
		(defq info (. this :resolve_path path))
		(if (not info)
			:nil
			(progn
				;; Extract filename from path
				;;rfind returns slice-compatible index (char after the match)
				(defq last_slash (rfind "/" path))
				(defq name (if last_slash
					(slice path last_slash -1)
					path))
				
				;; Unpack info: (first_cluster data_len attr)
				(bind '(first_cluster data_len attr) info)
				
				;; Check if directory
				(defq is_dir (= (logand attr +EXFAT_ATTR_DIR) +EXFAT_ATTR_DIR))
				
				;; Return (name size is_dir attr first_cluster)
				(list name data_len is_dir attr first_cluster)))
	)

	(defmethod :create_file (path)
		;; Create a new file at the given path
		;; Returns the new file's cluster number or :nil
		(raise :root_cluster)
		
		;; Parse path to get parent and filename
		(when (defq last_slash (rfind "/" path))
			;; Extract parent path and filename
			(defq parent_path (slice path 0 last_slash))
			(defq filename (slice path last_slash -1))
			
			;; Resolve parent directory
			(defq parent_cluster (if (or (= (length parent_path) 0) (eql parent_path "/"))
				root_cluster
				(first (. this :resolve_path parent_path))))
			
			;; Allocate cluster for new file
			(defq file_cluster (. this :alloc_cluster))
			
			;; Create the file entry
			(. this :create_entry parent_cluster filename +EXFAT_ATTR_ARCHIVE file_cluster 0)
			
			file_cluster)
	)

	(defmethod :delete_file (path)
		;; Delete a file at the given path or return :nil
		(raise :root_cluster)
		
		;; Parse path to get parent and filename
		(when (defq last_slash (rfind "/" path))
			;; Extract parent path and filename
			(defq parent_path (slice path 0 last_slash))
			(defq filename (slice path last_slash -1))
			
			;; Resolve parent directory
			(defq parent_cluster (if (or (= (length parent_path) 0) (eql parent_path "/"))
				root_cluster
				(first (. this :resolve_path parent_path))))
			
			;; Delete the file entry
			(. this :delete_entry parent_cluster filename)
			
			this)
	)

	(defmethod :delete_dir (path)
		;; Delete a directory at the given path
		;; Reuses delete_file logic as delete_entry handles both
		(. this :delete_file path)
	)

	(defmethod :update_entry_size (dir_cluster name new_size)
		(raise :cluster_size)
		(defq current_cluster dir_cluster)
		(defq found :nil)
		
		(while (and (< current_cluster 0xFFFFFFF8) (not found))
			(defq data (. this :read_cluster current_cluster))
			(defq idx 0)
			(while (and (< idx cluster_size) (not found))
				(defq type (get-ubyte data idx))
				
				(when (= type +EXFAT_TYPE_FILE)
					(defq stream_offset (+ idx 32))
					(defq name_offset (+ idx 64))
					
					(when (< name_offset cluster_size)
						(defq name_len (get-ubyte data (+ stream_offset 3)))
						(defq entry_name (get-str data (+ name_offset 2) (* name_len 2)))
						
						(defq match :t)
						(defq i 0)
						(if (/= name_len (length name))
							(setq match :nil)
							(while (< i name_len)
								(defq char_code (get-ushort entry_name (* i 2)))
								(defq name_char (code name 1 i))
								(when (/= char_code name_char)
									(setq match :nil))
								(setq i (+ i 1))
							)
						)
						
						(when match
							;; Update DataLength in Stream Extension Entry
							;; Offset 24 in Stream Extension (idx + 32 + 24)
							(set-long data (+ stream_offset 24) new_size)
							
							;; CRITICAL: Recalculate Set Checksum after modifying the entry
							;; The checksum is over the entire 96-byte entry set (File + Stream + Name)
							;; Skip bytes 2-3 (the checksum field itself)
							(defq set_checksum 0)
							(setq i 0)
							(while (< i 96)
								(if (or (= i 2) (= i 3))
									:nil
									(progn
										(defq byte (get-ubyte data (+ idx i)))
										(setq set_checksum (+ (logior (<< set_checksum 15) (>> set_checksum 1)) byte))
										(setq set_checksum (logand set_checksum 0xFFFF))))
								(setq i (+ i 1)))
							
							;; Update the Set Checksum in the File Entry (offset 2-3)
							(set-short data (+ idx 2) set_checksum)
							
							(. this :write_cluster current_cluster data)
							(setq found :t)
						)
					)
				)
				(setq idx (+ idx 32))
			)
			(setq current_cluster (. this :get_fat_entry current_cluster))
		)
		found
	)

	;; File Handle Operations
	(defmethod :open_file (path mode)
		;; Open a file and return handle ID (or :nil if invalid)
		;; mode: :read, :write, :append
		(raise :handles :next_handle)
		(when (defq stat_result (. this :stat path))
			(bind '(name size is_dir attr cluster) stat_result)
			(when (not is_dir)
				(defq position (if (eql mode :append) size 0))
				(defq handle_id next_handle)
				(set this :next_handle (+ next_handle 1))
				
				;; Store handle state: (list path cluster size position mode)
				(. handles :insert handle_id (list path cluster size position mode))
				handle_id))
	)

	(defmethod :close_file (handle_id)
		;; Close file handle - remove from handles map
		(raise :handles :root_cluster)
		(when (defq handle (. handles :find handle_id))
			(bind '(path cluster file_size position mode) handle)
			
			;; If write/append, update directory entry
			(when (or (eql mode :write) (eql mode :append))
				;; Parse path
				(when (defq last_slash (rfind "/" path))
					(defq parent_path (slice path 0 last_slash))
					(defq filename (slice path last_slash -1))
					
					(defq parent_cluster (if (or (= (length parent_path) 0) (eql parent_path "/"))
						root_cluster
						(first (. this :resolve_path parent_path))))
					
					;; Update entry in parent directory
					(. this :update_entry_size parent_cluster filename file_size)))
			
			(. handles :erase handle_id))
		this
	)

	(defmethod :read_file (handle_id size)
		;; Read data from file at current position
		(raise :cluster_size :handles)
		(when (defq handle (. handles :find handle_id))
			(bind '(path cluster file_size position mode) handle)
			
			;; Clamp size to remaining data
			(defq remaining (- file_size position))
			(if (<= remaining 0)
				""
				(progn
					(setq size (min size remaining))
					
					;; Calculate starting cluster and offset
					(defq cluster_offset (% position cluster_size))
					(defq clusters_to_skip (/ position cluster_size))
					
					;; Skip to starting cluster
					(defq current_cluster cluster)
					(defq i 0)
					(while (< i clusters_to_skip)
						(setq current_cluster (. this :get_fat_entry current_cluster))
						(setq i (+ i 1)))
					
					;; Read data
					(defq result (str-alloc size))
					(defq bytes_read 0)
					
					(while (< bytes_read size)
						(defq cluster_data (. this :read_cluster current_cluster))
						(defq start_offset (if (= bytes_read 0) cluster_offset 0))
						(defq bytes_to_copy (min (- cluster_size start_offset) (- size bytes_read)))
						
						;; Copy bytes
						(defq src_data (slice cluster_data start_offset (+ start_offset bytes_to_copy)))
						(defq j 0)
						(while (< j bytes_to_copy)
							(set-byte result (+ bytes_read j) (get-ubyte src_data j))
							(setq j (+ j 1)))
						
						(setq bytes_read (+ bytes_read bytes_to_copy))
						
						;; Move to next cluster if needed
						(when (< bytes_read size)
							(setq current_cluster (. this :get_fat_entry current_cluster))))
					
					;; Update position in handle
					(defq new_handle (list path cluster file_size (+ position size) mode))
					(. handles :insert handle_id new_handle)
					result)))
	)

	(defmethod :write_file (handle_id data)
		;; Write data to file at current position
		(raise :cluster_size :handles)
		(when (defq handle (. handles :find handle_id))
			(bind '(path cluster file_size position mode) handle)
			
			(defq data_len (length data))
			(defq bytes_written 0)
			
			;; Calculate starting cluster and offset
			(defq cluster_offset (% position cluster_size))
			(defq clusters_to_skip (/ position cluster_size))
			
			;; Traverse to starting cluster
			(defq current_cluster cluster)
			(defq i 0)
			(while (< i clusters_to_skip)
				(defq next_cluster (. this :get_fat_entry current_cluster))
				(if (or (= next_cluster 0) (>= next_cluster 0xFFFFFFF8))
					(progn
						;; Extend chain
						(defq new_cluster (. this :alloc_cluster))
						(. this :set_fat_entry current_cluster new_cluster)
						(. this :set_fat_entry new_cluster 0xFFFFFFFF)
						(setq current_cluster new_cluster))
					(setq current_cluster next_cluster))
				(setq i (+ i 1)))
			
			;; Write data loop
			(while (< bytes_written data_len)
				;; Read current cluster (for RMW)
				(defq cluster_data (. this :read_cluster current_cluster))
				
				;; Calculate write parameters
				(defq start_offset (if (= bytes_written 0) cluster_offset 0))
				(defq bytes_to_write (min (- cluster_size start_offset) (- data_len bytes_written)))
				
				;; Update cluster data
				(defq j 0)
				(while (< j bytes_to_write)
					(set-byte cluster_data (+ start_offset j) (get-ubyte data (+ bytes_written j)))
					(setq j (+ j 1)))
				
				;; Write back
				(. this :write_cluster current_cluster cluster_data)
				
				(setq bytes_written (+ bytes_written bytes_to_write))
				
				;; Move to next cluster if needed
				(when (< bytes_written data_len)
					(defq next_cluster (. this :get_fat_entry current_cluster))
					(if (or (= next_cluster 0) (>= next_cluster 0xFFFFFFF8))
						(progn
							;; Extend chain
							(defq new_cluster (. this :alloc_cluster))
							(. this :set_fat_entry current_cluster new_cluster)
							(. this :set_fat_entry new_cluster 0xFFFFFFFF)
							(setq current_cluster new_cluster))
						(setq current_cluster next_cluster))))
			
			;; Update handle
			(defq new_pos (+ position data_len))
			(defq new_size (max file_size new_pos))
			(defq new_handle (list path cluster new_size new_pos mode))
			(. handles :insert handle_id new_handle)
			
			bytes_written)
	)

	(defmethod :seek_file (handle_id offset whence)
		;; Seek to position in file
		;; whence: 0=SEEK_SET, 1=SEEK_CUR, 2=SEEK_END
		(raise :handles)
		(when (defq handle (. handles :find handle_id))
			(bind '(path cluster file_size current_pos mode) handle)
			
			(defq new_pos (cond
				((= whence 0) offset)              ;; SEEK_SET
				((= whence 1) (+ current_pos offset))  ;; SEEK_CUR
				((= whence 2) (+ file_size offset))    ;; SEEK_END
				:nil))
			
			(when new_pos
				;; Clamp to valid range
				(when (< new_pos 0) (setq new_pos 0))
				(when (> new_pos file_size) (setq new_pos file_size))
				
				;; Update handle with new position
				(defq new_handle (list path cluster file_size new_pos mode))
				(. handles :insert handle_id new_handle)
				new_pos))
	)

	;; Cluster Helpers
	(defmethod :cluster_to_offset (cluster)
		(raise :heap_offset :sector_size :cluster_size)
		(+ (* heap_offset sector_size) (* (- cluster 2) cluster_size))
	)

	(defmethod :get_fat_entry (cluster)
		(raise :stream :fat_offset :sector_size)
		(stream-seek stream (+ (* fat_offset sector_size) (* cluster 4)) 0)
		(get-uint (read-blk stream 4) 0)
	)

	(defmethod :set_fat_entry (cluster value)
		(raise :stream :fat_offset :sector_size)
		(stream-seek stream (+ (* fat_offset sector_size) (* cluster 4)) 0)
		(write-int stream value)
	)

	(defmethod :read_cluster (cluster)
		(raise :stream :cluster_size)
		(stream-seek stream (. this :cluster_to_offset cluster) 0)
		(read-blk stream cluster_size)
	)

	(defmethod :write_cluster (cluster data)
		(raise :stream)
		(stream-seek stream (. this :cluster_to_offset cluster) 0)
		(write-blk stream data)
	)

	(defmethod :alloc_cluster ()
		(raise :stream :bitmap_cluster :cluster_count)
		;; Read Bitmap
		(defq bitmap (str-alloc (/ (+ cluster_count 7) 8)))
		(stream-seek stream (. this :cluster_to_offset bitmap_cluster) 0)
		(defq bitmap_data (read-blk stream (length bitmap)))
		
		;; Find free bit
		(defq found_cluster 0)
		(defq idx 0)
		(while (and (< idx (length bitmap_data)) (= found_cluster 0))
			(defq byte (get-ubyte bitmap_data idx))
			(if (/= byte 0xFF)
				(progn
					(defq bit 0)
					(while (and (< bit 8) (= found_cluster 0))
						(if (= (logand byte (<< 1 bit)) 0)
							(progn
								;; Found free bit
								(setq found_cluster (+ (* idx 8) bit 2))
								;; Mark as used
								(set-byte bitmap_data idx (logior byte (<< 1 bit)))
								;; Write back bitmap sector(s) - optimizing to write just the changed byte would be better but for now write all
								(stream-seek stream (. this :cluster_to_offset bitmap_cluster) 0)
								(write-blk stream bitmap_data)
							)
						)
						(setq bit (+ bit 1))
					)
				)
			)
			(setq idx (+ idx 1))
		)
		(if (= found_cluster 0) (throw "Disk Full" this))
		found_cluster
	)

	(defmethod :free_cluster (cluster)
		(raise :stream :bitmap_cluster :cluster_count)
		;; Read Bitmap
		(defq bitmap_size (/ (+ cluster_count 7) 8))
		(stream-seek stream (. this :cluster_to_offset bitmap_cluster) 0)
		(defq bitmap_data (read-blk stream bitmap_size))
		
		(defq idx (/ (- cluster 2) 8))
		(defq bit (% (- cluster 2) 8))
		
		(defq byte (get-ubyte bitmap_data idx))
		(set-byte bitmap_data idx (logand byte (lognot (<< 1 bit))))
		
		(stream-seek stream (. this :cluster_to_offset bitmap_cluster) 0)
		(write-blk stream bitmap_data)
		this
	)

	(defmethod :format (size_mb stream)
		(defq size (* size_mb 1048576))
		(defq mstream (ifn stream (memory-stream)))
		(lower (:stream mstream))
		
		;; Zero out the disk
		(defq chunk_size 65536)
		(defq zeros (pad "" chunk_size (ascii-char 0)))
		(defq written 0)
		(while (< written size)
			(defq to_write (min chunk_size (- size written)))
			(write-blk mstream (if (= to_write chunk_size) zeros (slice zeros 0 to_write)))
			(setq written (+ written to_write))
		)
		
		;; Basic Geometry
		(lower
			(:sector_size (defq sector_size 512))
			(:cluster_size (defq cluster_size 4096))
			(:sectors_per_cluster (defq sectors_per_cluster 8))
			(:total_sectors (defq total_sectors (/ size sector_size)))
		)
		
		;; Layout
		(lower
			(:fat_offset (defq fat_offset 128))
			(:fat_length (defq fat_length (+ (/ (* (/ total_sectors sectors_per_cluster) 4) sector_size) 1)))
		)
		
		(lower
			(:heap_offset (defq heap_offset (+ fat_offset fat_length)))
			(:cluster_count (defq cluster_count (/ (- total_sectors heap_offset) sectors_per_cluster)))
			(:root_cluster (defq root_cluster 5))
			(:bitmap_cluster (defq bitmap_cluster 2))
			(:upcase_cluster (defq upcase_cluster 3))
		)
		
		;; Prepare Boot Sector
		(stream-seek mstream 0 0)
		(defq boot_sector (setf-> (str-alloc +exfat_boot_size)
			(+exfat_boot_jump_boot (hex-decode "EB7690"))
			(+exfat_boot_fs_name "EXFAT   ")
			(+exfat_boot_partition_offset 0)
			(+exfat_boot_volume_length total_sectors)
			(+exfat_boot_fat_offset fat_offset)
			(+exfat_boot_fat_length fat_length)
			(+exfat_boot_heap_offset heap_offset)
			(+exfat_boot_cluster_count cluster_count)
			(+exfat_boot_root_cluster root_cluster)
			(+exfat_boot_serial_number 0x12345678)
			(+exfat_boot_fs_revision 0x0100)
			(+exfat_boot_volume_flags 0)
			(+exfat_boot_bytes_per_sector_shift 9)
			(+exfat_boot_sectors_per_cluster_shift 3)
			(+exfat_boot_num_fats 1)
			(+exfat_boot_drive_select 0x80)
			(+exfat_boot_percent_in_use 0)
			(+exfat_boot_signature 0xAA55)
		))
		(write-blk mstream boot_sector)
		
		;; Extended Boot Sectors (8 sectors, zeroed for now but should be part of checksum)
		(defq extended_boot (pad "" (* 8 sector_size) (ascii-char 0)))
		(write-blk mstream extended_boot)
		
		;; OEM Parameters (1 sector, zeroed)
		(defq oem_params (pad "" sector_size (ascii-char 0)))
		(write-blk mstream oem_params)
		
		;; Reserved (1 sector, zeroed)
		(defq reserved_sector (pad "" sector_size (ascii-char 0)))
		(write-blk mstream reserved_sector)
		
		;; Calculate Checksum over the first 11 sectors (Boot + 8 Ext + 1 OEM + 1 Reserved)
		;; Algorithm:
		;; UINT32 Checksum = 0;
		;; for (i = 0; i < 11 * BytesPerSector; i++) {
		;;    if (i == 106 || i == 107 || i == 112) continue;
		;;    Checksum = ((Checksum << 31) | (Checksum >> 1)) + Data[i];
		;; }
		
		(defq checksum 0)
		(defq i 0)
		;; Boot Sector (Sector 0)
		(while (< i sector_size)
			(when (and (/= i 106) (/= i 107) (/= i 112))
				(defq byte (get-ubyte boot_sector i))
				(setq checksum (+ (logior (<< checksum 31) (>> checksum 1)) byte))
				(setq checksum (logand checksum 0xFFFFFFFF))
			)
			(setq i (+ i 1))
		)
		
		;; Extended Boot Sectors (Sectors 1-8) - all zeros
		(setq i 0)
		(while (< i (* 8 sector_size))
			(setq checksum (+ (logior (<< checksum 31) (>> checksum 1)) 0))
			(setq checksum (logand checksum 0xFFFFFFFF))
			(setq i (+ i 1))
		)
		
		;; OEM + Reserved (Sectors 9-10) - all zeros
		(setq i 0)
		(while (< i (* 2 sector_size))
			(setq checksum (+ (logior (<< checksum 31) (>> checksum 1)) 0))
			(setq checksum (logand checksum 0xFFFFFFFF))
			(setq i (+ i 1))
		)
		
		;; Write Checksum Sector (Sector 11)
		;; Fill sector with checksum
		(defq checksum_sector (pad "" sector_size (ascii-char 0)))
		(setq i 0)
		(while (< i sector_size)
			(set-int checksum_sector i checksum)
			(setq i (+ i 4))
		)
		(write-blk mstream checksum_sector)
		
		;; Write FAT (First 2 entries reserved)
		(stream-seek mstream (* fat_offset sector_size) 0)
		(write-blk mstream (setf-> (str-alloc +fat_reserved_size)
			(+fat_reserved_entry0 0xFFFFFFF8)
			(+fat_reserved_entry1 0xFFFFFFFF)
		))
		
		;; Write FAT entries for Bitmap (2), Upcase (3,4), Root (5)
		;; Cluster 2 (Bitmap) -> EOC
		(. this :set_fat_entry 2 0xFFFFFFFF)
		;; Cluster 3 (Upcase) -> 4
		(. this :set_fat_entry 3 4)
		;; Cluster 4 (Upcase) -> EOC
		(. this :set_fat_entry 4 0xFFFFFFFF)
		;; Cluster 5 (Root) -> EOC
		(. this :set_fat_entry 5 0xFFFFFFFF)
		
		;; Write Bitmap (Clusters 2,3,4,5 used)
		(defq heap_byte_offset (* heap_offset sector_size))
		(defq cl2_offset (+ heap_byte_offset (* (- bitmap_cluster 2) cluster_size)))
		(stream-seek mstream cl2_offset 0)
		(write-blk mstream (setf-> (str-alloc cluster_size) (+exfat_bitmap_data_val 0x0F)))
		
		;; Write Root Directory
		(defq root_offset (+ heap_byte_offset (* (- root_cluster 2) cluster_size)))
		(stream-seek mstream root_offset 0)
		
		;; Bitmap Entry
		(write-blk mstream (setf-> (str-alloc +exfat_bitmap_size)
			(+exfat_entry_header_type +EXFAT_TYPE_BITMAP)
			(+exfat_bitmap_flags 0)
			(+exfat_bitmap_first_cluster bitmap_cluster)
			(+exfat_bitmap_data_length (/ (+ cluster_count 7) 8))
		))
		
		;; Upcase Entry
		(write-blk mstream (setf-> (str-alloc +exfat_upcase_size)
			(+exfat_entry_header_type +EXFAT_TYPE_UPCASE)
			(+exfat_upcase_checksum 0)
			(+exfat_upcase_first_cluster upcase_cluster)
			(+exfat_upcase_data_length 0)
		))
		
		;; Volume Label Entry - "CHRYSALISP"
		(defq label_entry (str-alloc +exfat_label_size))
		(set-ubyte label_entry +exfat_entry_header_type +EXFAT_TYPE_LABEL)
		(set-ubyte label_entry +exfat_label_char_count 10)
		(defq label_name "CHRYSALISP")
		(defq i 0)
		(while (< i 10)
			(set-ushort label_entry (+ +exfat_label_volume_label (* i 2)) (code label_name 1 i))
			(setq i (+ i 1)))
		(write-blk mstream label_entry)
		
		(stream-flush mstream)
		this
	)
)