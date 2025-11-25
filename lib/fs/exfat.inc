(import "./fs.inc")
(import "lib/collections/fmap.inc")

;; ExFAT Constants
(defq +EXFAT_SECTOR_SIZE 512)
(defq +EXFAT_SECTOR_BITS 9)
(defq +EXFAT_CLUSTER_SIZE 4096)
(defq +EXFAT_CLUSTER_BITS 12)
(defq +EXFAT_SECTORS_PER_CLUSTER 8)

;; Boot Region Layout
(defq +EXFAT_BOOT_SECTOR 0)
(defq +EXFAT_EXTENDED_BOOT_SECTORS 8)
(defq +EXFAT_OEM_SECTOR 9)
(defq +EXFAT_RESERVED_SECTOR 10)
(defq +EXFAT_CHECKSUM_SECTOR 11)
(defq +EXFAT_BACKUP_BOOT_SECTOR 12)
(defq +EXFAT_BOOT_REGION_SIZE 24)

;; Directory Entry Size
(defq +EXFAT_ENTRY_SIZE 32)

;; Cluster Constants
(defq +EXFAT_ROOT_DIR_CLUSTER 2)
(defq +EXFAT_FIRST_DATA_CLUSTER 2)

;; FAT Entry Values
(defq +EXFAT_FAT_FREE 0x00000000)
(defq +EXFAT_FAT_BAD 0xFFFFFFF7)
(defq +EXFAT_FAT_EOC 0xFFFFFFFF)
(defq +EXFAT_FAT_MEDIA_TYPE 0xFFFFFFF8)

;; Boot Signature
(defq +EXFAT_BOOT_SIGNATURE 0xAA55)

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

;; Stream Extension Flags
(defq +EXFAT_STREAM_ALLOC_POSSIBLE 0x01)
(defq +EXFAT_STREAM_NO_FAT_CHAIN   0x02)

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

	(defmethod :format (stream size_mb)
		;; Format an exFat volume
		;; (. exfat :format stream size_mb) -> this
		(lower (:stream stream))
		(defq size_sectors (>> (* size_mb 1048576) +EXFAT_SECTOR_BITS))
		(defq cluster_count (>> size_sectors 3))
		
		;; Calculate FAT size (4 bytes per cluster entry)
		(defq fat_length_sectors (>> (+ (* cluster_count 4) +EXFAT_SECTOR_SIZE -1) +EXFAT_SECTOR_BITS))
		
		;; FAT offset should be 128 sectors (like macOS) to leave room for boot region
		(defq fat_offset_sectors 128)
		
		;; Calculate bitmap size (1 bit per cluster)
		(defq bitmap_bytes (>> (+ cluster_count 7) 3))
		(defq bitmap_clusters (>> (+ bitmap_bytes +EXFAT_CLUSTER_SIZE -1) +EXFAT_CLUSTER_BITS))
		
		;; Root dir is at cluster after bitmap
		(defq root_dir_cluster (+ +EXFAT_ROOT_DIR_CLUSTER bitmap_clusters))

		;; Store volume parameters
		(lower
			(:size_sectors size_sectors)
			(:cluster_count cluster_count)
			(:fat_offset fat_offset_sectors)
			(:fat_length fat_length_sectors)
			(:heap_offset (+ fat_offset_sectors fat_length_sectors))
			(:root_cluster root_dir_cluster)
			(:bitmap_clusters bitmap_clusters)
			(:bitmap_length bitmap_bytes))
		
		;; Write MBR (Master Boot Record) at sector 0
		(. this :write_mbr)
		
		;; Write boot sector at sector 1
		(. this :write_boot_sector)
		
		;; Write extended boot sectors
		(. this :write_extended_boot_sectors)
		
		;; Write OEM and reserved sectors
		(. this :write_oem_reserved_sectors)
		
		;; Write boot checksum
		(. this :write_boot_checksum)
		
		;; Write backup boot region
		(. this :write_backup_boot_region)
		
		;; Initialize FAT
		(. this :init_fat)
		
		;; Create allocation bitmap
		(. this :create_bitmap bitmap_clusters)
		
		;; Create up-case table
		(. this :create_upcase_table)
		
		;; Create root directory
		(. this :create_root_directory)
		
		;; Flush stream
		(stream-flush stream)
		this)

	(defmethod :write_mbr ()
		;; Write MBR with single exFAT partition
		(raise :stream :size_sectors)
		(stream-seek stream 0 0)
		
		;; Create MBR (512 bytes)
		(defq mbr (pad "" +EXFAT_SECTOR_SIZE (ascii-char 0)))
		
		;; Partition entry at offset 0x1BE (446)
		;; Boot indicator (0x00 = not bootable)
		(set-byte mbr 446 0x00)
		;; Start CHS (0xFE 0xFF 0xFF = use LBA)
		(set-byte mbr 447 0xFE)
		(set-byte mbr 448 0xFF)
		(set-byte mbr 449 0xFF)
		;; Partition type (0x07 = exFAT/NTFS)
		(set-byte mbr 450 0x07)
		;; End CHS (0xFE 0xFF 0xFF = use LBA)
		(set-byte mbr 451 0xFE)
		(set-byte mbr 452 0xFF)
		(set-byte mbr 453 0xFF)
		;; LBA of first sector (1 = start at sector 1)
		(set-int mbr 454 1)
		;; Number of sectors in partition (size_sectors - 1)
		(set-int mbr 458 (- size_sectors 1))
		
		;; MBR signature
		(set-short mbr 510 0xAA55)
		
		(write-blk stream mbr))

	(defmethod :write_boot_sector ()
		;; Write the main boot sector at sector 1 (after MBR)
		(raise :stream :size_sectors :cluster_count :fat_offset :fat_length :heap_offset :root_cluster)
		(stream-seek stream +EXFAT_SECTOR_SIZE 0)
		
		;; Create boot sector with code section filled with 0xF4 (HLT instruction)
		(defq boot (pad "" +exfat_boot_size (ascii-char 0xF4)))
		
		(write-blk stream (setf-> boot
			(+exfat_boot_jump_boot (hex-decode "EB7690"))
			(+exfat_boot_fs_name "EXFAT   ")
			(+exfat_boot_partition_offset 1)
			(+exfat_boot_volume_length size_sectors)
			(+exfat_boot_fat_offset fat_offset)
			(+exfat_boot_fat_length fat_length)
			(+exfat_boot_heap_offset heap_offset)
			(+exfat_boot_cluster_count cluster_count)
			(+exfat_boot_root_cluster root_cluster)
			(+exfat_boot_serial_number 0x12345678)
			(+exfat_boot_fs_revision 0x0100)
			(+exfat_boot_volume_flags 0)
			(+exfat_boot_bytes_per_sector_shift +EXFAT_SECTOR_BITS)
			(+exfat_boot_sectors_per_cluster_shift 3)
			(+exfat_boot_num_fats 1)
			(+exfat_boot_drive_select 0x80)
			(+exfat_boot_percent_in_use 0)
			(+exfat_boot_signature +EXFAT_BOOT_SIGNATURE))))

	(defmethod :write_extended_boot_sectors ()
		;; Write 8 extended boot sectors with signatures
		(raise :stream)
		(defq sector (pad "" +EXFAT_SECTOR_SIZE (ascii-char 0)))
		(set-short sector (- +EXFAT_SECTOR_SIZE 2) 0xAA55)
		(each (lambda (_)
			(write-blk stream sector))
			(list 0 1 2 3 4 5 6 7)))

	(defmethod :write_oem_reserved_sectors ()
		;; Write OEM parameters (sector 9) and reserved sector (sector 10)
		(raise :stream)
		(defq sector (pad "" +EXFAT_SECTOR_SIZE (ascii-char 0)))
		(write-blk stream sector)
		(write-blk stream sector))

	(defmethod :write_boot_checksum ()
		;; Calculate and write boot checksum (sector 11)
		;; Checksum is calculated over sectors 1-10 (boot + extended + OEM + reserved)
		;; Excluding bytes at offsets 106, 107, 112 in the boot sector
		(raise :stream)
		
		;; Read back the boot region (sectors 1-10) 
		(stream-seek stream +EXFAT_SECTOR_SIZE 0)
		(defq boot_region (read-blk stream (* 11 +EXFAT_SECTOR_SIZE)))
		
		;; Calculate checksum
		(defq checksum 0)
		(defq idx 0)
		(defq region_size (* 11 +EXFAT_SECTOR_SIZE))
		
		(while (< idx region_size)
			;; Skip volume_flags and percent_in_use fields
			;; volume_flags at offset 106-107 (in first sector)
			;; percent_in_use at offset 112 (in first sector)
			(when (and (not (or (= idx 106) (= idx 107) (= idx 112))))
				(defq byte_val (get-ubyte boot_region idx))
				(setq checksum (logand (+ (logior (<< checksum 31) (>>> checksum 1)) byte_val) 0xFFFFFFFF)))
			(++ idx))
		
		;; Write checksum sector - repeat the 32-bit checksum 128 times
		(defq checksum_sector (str-alloc +EXFAT_SECTOR_SIZE))
		(defq idx 0)
		(while (< idx +EXFAT_SECTOR_SIZE)
			(set-int checksum_sector idx checksum)
			(setq idx (+ idx 4)))
		
		;; Seek to sector 12 (MBR + 11 sectors) and write
		(stream-seek stream (* 12 +EXFAT_SECTOR_SIZE) 0)
		(write-blk stream checksum_sector))

	(defmethod :write_backup_boot_region ()
		;; Copy boot region to backup location
		;; Main boot: sectors 1-12 (exFat boot + extended + OEM + reserved + checksum)
		;; Backup boot: sectors 13-24 (exact copy)
		(raise :stream)
		
		;; Read main boot region (sectors 1-12, NOT including MBR at sector 0)
		(stream-seek stream +EXFAT_SECTOR_SIZE 0)
		(defq boot_region (read-blk stream (* 12 +EXFAT_SECTOR_SIZE)))
		
		;; Write to backup location (sector 13)
		(stream-seek stream (* 13 +EXFAT_SECTOR_SIZE) 0)
		(write-blk stream boot_region))

	(defmethod :init_fat ()
		;; Initialize FAT with reserved entries
		(raise :stream :fat_offset)
		(stream-seek stream (* fat_offset +EXFAT_SECTOR_SIZE) 0)
		
		;; Write FAT entry 0 (media type) and entry 1 (0xFFFFFFFF)
		(write-blk stream (setf-> (str-alloc +fat_reserved_size)
			(+fat_reserved_entry0 +EXFAT_FAT_MEDIA_TYPE)
			(+fat_reserved_entry1 +EXFAT_FAT_EOC))))

	(defmethod :create_bitmap (bitmap_clusters)
		;; Create allocation bitmap in root directory
		;; Bitmap starts at cluster 2, root dir will be after
		(raise :stream :heap_offset)
		
		;; Seek to cluster 2
		(defq cluster_offset (+ (* heap_offset +EXFAT_SECTOR_SIZE) 
			(* (- +EXFAT_ROOT_DIR_CLUSTER 2) +EXFAT_CLUSTER_SIZE)))
		(stream-seek stream cluster_offset 0)
		
		;; Write bitmap data - mark first few clusters as allocated
		(defq allocated_clusters (+ bitmap_clusters 2))
		(defq bitmap (pad "" +EXFAT_CLUSTER_SIZE (ascii-char 0)))
		
		;; Mark clusters 0-allocated_clusters as used
		(each (lambda (i)
			(defq byte_idx (>> i 3))
			(defq bit_idx (logand i 7))
			(defq byte_val (get-ubyte bitmap byte_idx))
			(set-byte bitmap byte_idx (logior byte_val (<< 1 bit_idx))))
			(map (lambda (i) i) (list 0 allocated_clusters)))
		
		(write-blk stream bitmap)
		
		;; Update FAT for bitmap clusters
		(. this :set_fat_chain +EXFAT_ROOT_DIR_CLUSTER (+ +EXFAT_ROOT_DIR_CLUSTER bitmap_clusters -1)))

	(defmethod :create_upcase_table ()
		;; For now, skip upcase table creation - can add later
		:nil)

	(defmethod :create_root_directory ()
		;; Create root directory with volume label and bitmap entry
		(raise :stream :heap_offset :root_cluster :bitmap_length)
		
		(defq cluster_offset (+ (* heap_offset +EXFAT_SECTOR_SIZE)
			(* (- root_dir_cluster 2) +EXFAT_CLUSTER_SIZE)))
		(stream-seek stream cluster_offset 0)
		
		;; 1. Create Volume Label Entry
		(defq label "CHRYSALISP")
		(defq label_entry (str-alloc +exfat_label_size))
		;; Set type field (at offset 0, inherited from entry_header)
		(set-byte label_entry 0 +EXFAT_TYPE_LABEL)
		;; Set char_count field
		(setf label_entry +exfat_label_char_count (length label))
		
		;; Convert label to UTF-16LE
		(each (lambda (c)
			(set-short label_entry (+ +exfat_label_label (* (!) 2)) (ascii-code c)))
			label)
		
		(write-blk stream label_entry)
		
		;; 2. Create Allocation Bitmap Entry
		(defq bitmap_entry (str-alloc +exfat_bitmap_size))
		(set-byte bitmap_entry 0 +EXFAT_TYPE_BITMAP)
		(set-byte bitmap_entry +exfat_bitmap_flags 0) ;; First bitmap (0)
		(set-int bitmap_entry +exfat_bitmap_first_cluster +EXFAT_ROOT_DIR_CLUSTER) ;; Bitmap is at cluster 2
		(set-long bitmap_entry +exfat_bitmap_data_length bitmap_length)
		
		(write-blk stream bitmap_entry)
		
		;; Write end-of-directory marker
		(write-blk stream (pad "" +EXFAT_ENTRY_SIZE (ascii-char 0)))
		
		;; Update FAT for root directory
		(. this :set_fat_entry root_cluster +EXFAT_FAT_EOC))

	(defmethod :set_fat_entry (cluster value)
		;; Set a FAT entry
		(raise :stream :fat_offset)
		(defq fat_pos (+ (* fat_offset +EXFAT_SECTOR_SIZE) (* cluster 4)))
		(stream-seek stream fat_pos 0)
		(write-int stream value))

	(defmethod :set_fat_chain (start_cluster end_cluster)
		;; Set a chain of FAT entries
		(defq cluster start_cluster)
		(while (< cluster end_cluster)
			(. this :set_fat_entry cluster (+ cluster 1))
			(++ cluster))
		(. this :set_fat_entry end_cluster +EXFAT_FAT_EOC))
)