;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Low-Level Hex Dumper
; Debug tool for inspecting raw filesystem data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defun byte_to_hex (byte)
	; Convert byte to 2-digit hex string
	(defq hex_chars "0123456789ABCDEF"
		high (>> byte 4)
		low (logand byte 0x0F))
	(cat (char (code hex_chars 1 high))
		(char (code hex_chars 1 low))))

(defun format_hex_dump (data offset bytes_per_line)
	; Format data as hex dump with offset, hex, and ASCII
	; Returns list of formatted lines
	(defq lines (list)
		data_len (length data)
		line_offset 0)

	(while (< line_offset data_len)
		; Build offset field
		(defq offset_str (str-from-num (+ offset line_offset) 16 8))

		; Build hex field
		(defq hex_str ""
			i 0)
		(while (< i bytes_per_line)
			(if (< (+ line_offset i) data_len)
				(progn
					(defq byte_val (code data 1 (+ line_offset i)))
					(setq hex_str (cat hex_str (byte_to_hex byte_val) " ")))
				(setq hex_str (cat hex_str "   ")))  ; Padding
			(setq i (inc i)))

		; Build ASCII field
		(defq ascii_str ""
			i 0)
		(while (and (< i bytes_per_line) (< (+ line_offset i) data_len))
			(defq byte_val (code data 1 (+ line_offset i)))
			(if (and (>= byte_val 32) (<= byte_val 126))  ; Printable ASCII
				(setq ascii_str (cat ascii_str (char byte_val)))
				(setq ascii_str (cat ascii_str ".")))
			(setq i (inc i)))

		; Combine into line
		(push lines (cat offset_str "  " hex_str " |" ascii_str "|"))

		(setq line_offset (+ line_offset bytes_per_line)))

	lines)

(defun dump_boot_sector (exfat_obj)
	; Dump boot sector with annotations
	(print)
	(print "Boot Sector Dump")
	(print "================")

	(defq stream_obj (get exfat_obj :stream)
		sector_size (get exfat_obj :sector_size))

	; Read boot sector
	(stream-seek stream_obj 0 0)
	(when-bind (boot_sector (read-blk stream_obj sector_size))
		; Show first 128 bytes with annotations
		(defq lines (format_hex_dump (slice boot_sector 0 128) 0 16))
		(each! (lambda (line) (print line)) lines)

		(print)
		(print "Key Fields:")
		(print "  0x00-0x02: Jump Boot (0xEB 0x76 0x90)")
		(print "  0x03-0x0A: FileSystem Name (\"EXFAT   \")")
		(print "  0x6A-0x74: Volume Label (11 bytes)")
		(print "  0x1FE-0x1FF: Signature (0x55 0xAA)")))

(defun dump_fat_entries (exfat_obj start_entry count)
	; Dump FAT entries in hex with interpretation
	(print)
	(print "FAT Entries Dump")
	(print "================")
	(print "Entry    Hex Value   Interpretation")
	(print "-------  ----------  ---------------")

	(each! (lambda (i)
			(defq cluster_num (+ start_entry i))
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_num))
				(defq hex_str (str-from-num entry 16 8)
					interpretation
					(cond
						((= entry 0x00000000) "Free")
						((= entry 0xFFFFFFF7) "Bad Cluster")
						((= entry 0xFFFFFFFF) "End of Chain")
						((and (>= entry 2) (< entry 0xFFFFFFF7))
							(cat "Next: " (str-from-num entry 10)))
						(:t "Reserved")))
				(print (str-from-num cluster_num 10 7) "  0x" hex_str "  " interpretation)))
		count 0))

(defun dump_cluster_hex (exfat_obj cluster_num)
	; Dump cluster contents in hex
	(print)
	(print "Cluster " cluster_num " Dump")
	(print (str-alloc (+ 14 (length (str-from-num cluster_num 10))) (char 0x3D)))

	(when-bind (cluster_data (. exfat_obj :read_cluster cluster_num))
		; Show first 256 bytes
		(defq dump_size (if (> (length cluster_data) 256) 256 (length cluster_data))
			lines (format_hex_dump (slice cluster_data 0 dump_size) 0 16))

		(each! (lambda (line) (print line)) lines)

		(when (> (length cluster_data) 256)
			(print "... (" (- (length cluster_data) 256) " more bytes)"))))

(defun dump_sector_hex (exfat_obj sector_num)
	; Dump sector contents in hex
	(print)
	(print "Sector " sector_num " Dump")
	(print (str-alloc (+ 13 (length (str-from-num sector_num 10))) (char 0x3D)))

	(when-bind (sector_data (. exfat_obj :read_sector sector_num))
		(defq lines (format_hex_dump sector_data (* sector_num (get exfat_obj :sector_size)) 16))
		(each! (lambda (line) (print line)) lines)))

(defun compare_hex_dumps (data1 data2 offset1 offset2 max_bytes)
	; Compare two data blocks and show differences
	(print)
	(print "Hex Comparison")
	(print "==============")

	(defq len1 (length data1)
		len2 (length data2)
		max_len (if (< len1 len2) len1 len2)
		compare_len (if (< max_len max_bytes) max_len max_bytes)
		differences 0)

	(each! (lambda (i)
			(defq byte1 (code data1 1 i)
				byte2 (code data2 1 i))
			(unless (= byte1 byte2)
				(print "Offset " (str-from-num (+ offset1 i) 16 8)
					": 0x" (byte_to_hex byte1)
					" vs 0x" (byte_to_hex byte2))
				(setq differences (inc differences))))
		compare_len 0)

	(print)
	(print "Total differences: " differences " out of " compare_len " bytes"))

(defun dump_fat_sector (exfat_obj sector_offset)
	; Dump a FAT sector showing all entries
	(print)
	(print "FAT Sector Dump (sector offset " sector_offset ")")
	(print "================================================")

	(defq fat_offset (get exfat_obj :fat_offset)
		sector_num (+ fat_offset sector_offset))

	(when-bind (sector_data (. exfat_obj :read_sector sector_num))
		; Each FAT entry is 4 bytes, so one sector has sector_size/4 entries
		(defq entries_per_sector (/ (get exfat_obj :sector_size) 4))

		(print "Entries in this sector: " entries_per_sector)
		(print)

		(each! (lambda (i)
				(defq offset (* i 4)
					entry_value (+ (code sector_data 1 offset)
						(<< (code sector_data 1 (+ offset 1)) 8)
						(<< (code sector_data 1 (+ offset 2)) 16)
						(<< (code sector_data 1 (+ offset 3)) 24))
					cluster_num (+ (* sector_offset entries_per_sector) i))

				(when (not (= entry_value 0))  ; Only show non-zero entries
					(print "  Cluster " (str-from-num cluster_num 10 6)
						": 0x" (str-from-num entry_value 16 8))))
			entries_per_sector 0)))

(defun dump_summary (exfat_obj)
	; Dump filesystem structure summary
	(print)
	(print "Filesystem Structure Summary")
	(print "============================")

	(defq sector_size (get exfat_obj :sector_size)
		cluster_size (get exfat_obj :cluster_size)
		fat_offset (get exfat_obj :fat_offset)
		fat_length (get exfat_obj :fat_length)
		cluster_heap_offset (get exfat_obj :cluster_heap_offset)
		cluster_count (get exfat_obj :cluster_count))

	(print "Boot Sector:")
	(print "  Location: Sector 0")
	(print "  Size: " sector_size " bytes")
	(print)

	(print "FAT Table:")
	(print "  Start: Sector " fat_offset " (offset 0x" (str-from-num (* fat_offset sector_size) 16) ")")
	(print "  Length: " fat_length " sectors (" (* fat_length sector_size) " bytes)")
	(print "  Entries: " cluster_count " (4 bytes each)")
	(print)

	(print "Cluster Heap:")
	(print "  Start: Sector " cluster_heap_offset " (offset 0x" (str-from-num (* cluster_heap_offset sector_size) 16) ")")
	(print "  Cluster Size: " cluster_size " bytes")
	(print "  Cluster Count: " cluster_count))

(defun main ()
	(print "ExFat Low-Level Hex Dumper")
	(print "==========================")
	(print)

	; Create demo filesystem
	(print "Creating demo filesystem...")
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)

	; Allocate some clusters to make dumps interesting
	(. exfat_obj :allocate_cluster)
	(. exfat_obj :allocate_cluster)

	; Write data to a cluster
	(defq test_cluster 2)
	(. exfat_obj :write_cluster test_cluster "Test data in cluster for hex dump")

	(print "Demo filesystem created")

	; Show various dumps
	(dump_summary exfat_obj)
	(dump_boot_sector exfat_obj)
	(dump_fat_entries exfat_obj 0 10)
	(dump_cluster_hex exfat_obj test_cluster)
	(dump_sector_hex exfat_obj 0)
	(dump_fat_sector exfat_obj 0)

	(print)
	(print "Hex Dumper Demo Complete")
	(print)
	(print "Usage in real application:")
	(print "  ; Dump boot sector")
	(print "  (dump_boot_sector my_exfat)")
	(print)
	(print "  ; Dump FAT entries")
	(print "  (dump_fat_entries my_exfat 0 20)")
	(print)
	(print "  ; Dump cluster contents")
	(print "  (dump_cluster_hex my_exfat 5)")

	0)

; Run the demo
(main)
