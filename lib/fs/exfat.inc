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
)