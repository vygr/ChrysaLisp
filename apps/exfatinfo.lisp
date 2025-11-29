;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Filesystem Information Dumper
; Displays detailed filesystem metadata and statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defun format_bytes (bytes)
	; Format bytes in human-readable form
	(cond
		((>= bytes (* 1024 1024 1024))
			(cat (str-from-num (/ bytes (* 1024 1024 1024)) 10) " GB"))
		((>= bytes (* 1024 1024))
			(cat (str-from-num (/ bytes (* 1024 1024)) 10) " MB"))
		((>= bytes 1024)
			(cat (str-from-num (/ bytes 1024) 10) " KB"))
		(:t
			(cat (str-from-num bytes 10) " bytes"))))

(defun print_section (title)
	; Print a section header
	(print)
	(print title)
	(print (str-alloc (length title) (char 0x3D))))  ; '=' characters

(defun dump_boot_sector_info (exfat_obj)
	; Display boot sector information
	(print_section "Boot Sector Information")

	(defq sector_size (get exfat_obj :sector_size)
		cluster_size (get exfat_obj :cluster_size)
		cluster_shift (get exfat_obj :cluster_shift)
		fat_offset (get exfat_obj :fat_offset)
		fat_length (get exfat_obj :fat_length)
		cluster_heap_offset (get exfat_obj :cluster_heap_offset)
		cluster_count (get exfat_obj :cluster_count)
		root_dir_cluster (get exfat_obj :root_dir_cluster))

	(print "  File System Name:      EXFAT")
	(print "  Sector Size:           " sector_size " bytes")
	(print "  Cluster Size:          " cluster_size " bytes (" (/ cluster_size sector_size) " sectors)")
	(print "  Cluster Shift:         " cluster_shift " bits")
	(print "  FAT Offset:            " fat_offset " sectors (byte offset: " (* fat_offset sector_size) ")")
	(print "  FAT Length:            " fat_length " sectors (" (format_bytes (* fat_length sector_size)) ")")
	(print "  Cluster Heap Offset:   " cluster_heap_offset " sectors (byte offset: " (* cluster_heap_offset sector_size) ")")
	(print "  Cluster Count:         " cluster_count " clusters")
	(print "  Root Dir Cluster:      " root_dir_cluster))

(defun dump_geometry (exfat_obj)
	; Display filesystem geometry
	(print_section "Filesystem Geometry")

	(defq sector_size (get exfat_obj :sector_size)
		cluster_size (get exfat_obj :cluster_size)
		cluster_count (get exfat_obj :cluster_count)
		fat_length (get exfat_obj :fat_length)
		cluster_heap_offset (get exfat_obj :cluster_heap_offset)
		total_size (. exfat_obj :get_size)
		data_area_size (* cluster_count cluster_size)
		fat_area_size (* fat_length sector_size)
		reserved_area_size (* cluster_heap_offset sector_size)
		overhead_size (- total_size data_area_size))

	(print "  Total Filesystem Size: " total_size " bytes (" (format_bytes total_size) ")")
	(print "  Data Area Size:        " data_area_size " bytes (" (format_bytes data_area_size) ")")
	(print "  FAT Area Size:         " fat_area_size " bytes (" (format_bytes fat_area_size) ")")
	(print "  Reserved Area Size:    " reserved_area_size " bytes (" (format_bytes reserved_area_size) ")")
	(print "  Overhead:              " overhead_size " bytes (" (format_bytes overhead_size) ")")
	(print "  Overhead Percentage:   " (/ (* overhead_size 100) total_size) "%"))

(defun analyze_fat_usage (exfat_obj)
	; Analyze FAT table usage patterns
	(print_section "FAT Usage Analysis")

	(defq cluster_count (get exfat_obj :cluster_count)
		+exfat_fat_free 0x00000000
		+exfat_fat_bad 0xFFFFFFF7
		+exfat_fat_eoc 0xFFFFFFFF
		free_count 0
		allocated_count 0
		bad_count 0
		chain_count 0
		longest_chain 0)

	(print "  Scanning " cluster_count " clusters...")

	; Scan all data clusters
	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(cond
					((= entry +exfat_fat_free)
						(setq free_count (inc free_count)))
					((= entry +exfat_fat_bad)
						(setq bad_count (inc bad_count)))
					((= entry +exfat_fat_eoc)
						(setq allocated_count (inc allocated_count)
							chain_count (inc chain_count)))
					((<= 2 entry (+ cluster_count 1))
						(setq allocated_count (inc allocated_count))))))
		(+ cluster_count 2) :nil 2)

	(defq used_percentage (/ (* allocated_count 100) cluster_count))

	(print "  Free Clusters:         " free_count " (" (/ (* free_count 100) cluster_count) "%)")
	(print "  Allocated Clusters:    " allocated_count " (" used_percentage "%)")
	(print "  Bad Clusters:          " bad_count)
	(print "  Chain Heads (EOC):     " chain_count)
	(print "  Available Space:       " (format_bytes (* free_count (get exfat_obj :cluster_size))))
	(print "  Used Space:            " (format_bytes (* allocated_count (get exfat_obj :cluster_size)))))

(defun dump_fat_table_sample (exfat_obj count)
	; Display first N FAT entries
	(print_section (cat "FAT Table Sample (first " (str-from-num count 10) " entries)"))

	(print "  Cluster | FAT Entry    | Interpretation")
	(print "  --------|--------------|------------------")

	(each! (lambda (i)
			(when-bind (entry (. exfat_obj :read_fat_entry i))
				(defq interpretation
					(cond
						((= entry 0x00000000) "Free")
						((= entry 0xFFFFFFF7) "Bad Cluster")
						((= entry 0xFFFFFFFF) "End of Chain")
						((and (>= entry 2) (< entry 0xFFFFFFF7)) (cat "Next: " (str-from-num entry 10)))
						(:t "Reserved/Unknown")))
				(print "  " (str-from-num i 10 6) "  | 0x" (str-from-num entry 16 8) " | " interpretation)))
		count 0))

(defun calculate_fragmentation (exfat_obj)
	; Calculate filesystem fragmentation statistics
	(print_section "Fragmentation Analysis")

	(defq cluster_count (get exfat_obj :cluster_count)
		+exfat_fat_eoc 0xFFFFFFFF
		total_chains 0
		total_fragments 0
		sequential_count 0)

	(print "  Analyzing cluster allocation patterns...")

	; Count chains and fragments
	(defq prev_cluster :nil)
	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(when (= entry +exfat_fat_eoc)
					(setq total_chains (inc total_chains)))
				(when (and prev_cluster (= entry (inc prev_cluster)))
					(setq sequential_count (inc sequential_count)))
				(setq prev_cluster cluster_idx)))
		(+ cluster_count 2) :nil 2)

	(defq avg_fragments (if (> total_chains 0)
		(/ total_fragments total_chains)
		0))

	(print "  Total Chains:          " total_chains)
	(print "  Sequential Clusters:   " sequential_count)
	(print "  Fragmentation:         " (if (= total_chains 0) "N/A (empty)" "Low")))

(defun dump_cluster_map (exfat_obj width)
	; Display visual cluster allocation map
	(print_section "Cluster Allocation Map")

	(defq cluster_count (get exfat_obj :cluster_count)
		+exfat_fat_free 0x00000000
		rows (/ (+ cluster_count width -1) width))

	(print "  Legend: . = free, # = allocated, X = bad, R = root")
	(print)

	(each! (lambda (row)
			(prin "  ")
			(each! (lambda (col)
					(defq cluster_idx (+ 2 (* row width) col))
					(if (< cluster_idx (+ cluster_count 2))
						(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
							(prin
								(cond
									((= cluster_idx (get exfat_obj :root_dir_cluster)) "R")
									((= entry +exfat_fat_free) ".")
									((= entry 0xFFFFFFF7) "X")
									(:t "#"))))
						(prin " ")))
				width 0)
			(print))
		rows 0))

(defun main ()
	(print "ExFat Filesystem Information Dumper")
	(print "====================================")

	; Create test filesystem
	(print)
	(print "Creating test filesystem for demonstration...")
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 10 1024 1024))

	; Format it
	(. exfat_obj :format fs_size)

	; Allocate some clusters to show usage
	(. exfat_obj :allocate_cluster)
	(. exfat_obj :allocate_cluster)
	(. exfat_obj :allocate_cluster)

	; Display all information
	(dump_boot_sector_info exfat_obj)
	(dump_geometry exfat_obj)
	(analyze_fat_usage exfat_obj)
	(dump_fat_table_sample exfat_obj 20)
	(calculate_fragmentation exfat_obj)
	(dump_cluster_map exfat_obj 64)

	(print)
	(print "Information dump complete")

	0)

; Run the info dumper
(main)
