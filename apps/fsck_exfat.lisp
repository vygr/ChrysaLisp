;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Filesystem Checker (fsck.exfat)
; Validates filesystem integrity and reports errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defun check_boot_sector (exfat_obj)
	; Check boot sector validity
	; Returns: list of (error_count warning_count)
	(defq errors 0
		warnings 0
		sector_size (get exfat_obj :sector_size)
		cluster_size (get exfat_obj :cluster_size)
		fat_offset (get exfat_obj :fat_offset)
		fat_length (get exfat_obj :fat_length)
		cluster_heap_offset (get exfat_obj :cluster_heap_offset)
		cluster_count (get exfat_obj :cluster_count))

	(print "Checking boot sector...")

	; Validate sector size is power of 2
	(unless (= (logand sector_size (- sector_size 1)) 0)
		(print "  ERROR: Sector size is not a power of 2")
		(setq errors (inc errors)))

	; Validate cluster size is power of 2
	(unless (= (logand cluster_size (- cluster_size 1)) 0)
		(print "  ERROR: Cluster size is not a power of 2")
		(setq errors (inc errors)))

	; Validate FAT offset is reasonable
	(when (< fat_offset 24)
		(print "  ERROR: FAT offset too small (< 24 sectors)")
		(setq errors (inc errors)))

	; Validate cluster heap follows FAT
	(unless (= cluster_heap_offset (+ fat_offset fat_length))
		(print "  ERROR: Cluster heap offset doesn't follow FAT")
		(print "    Expected: " (+ fat_offset fat_length))
		(print "    Got: " cluster_heap_offset)
		(setq errors (inc errors)))

	; Validate cluster count is reasonable
	(when (< cluster_count 2)
		(print "  ERROR: Cluster count too small")
		(setq errors (inc errors)))

	(if (= errors 0)
		(print "  Boot sector: OK")
		(print "  Boot sector: " errors " errors found"))

	(list errors warnings))

(defun check_fat_entries (exfat_obj)
	; Check FAT table consistency
	; Returns: list of (error_count warning_count)
	(defq errors 0
		warnings 0
		cluster_count (get exfat_obj :cluster_count)
		+exfat_fat_free 0x00000000
		+exfat_fat_bad 0xFFFFFFF7
		+exfat_fat_eoc 0xFFFFFFFF
		allocated_clusters (list)
		free_count 0)

	(print "Checking FAT entries...")

	; Check reserved entries (0 and 1)
	(defq entry_0 (. exfat_obj :read_fat_entry 0)
		entry_1 (. exfat_obj :read_fat_entry 1))

	(when entry_0
		(unless (= (logand entry_0 0xFF) 0xF8)
			(print "  WARNING: FAT entry 0 has unexpected value")
			(setq warnings (inc warnings))))

	; Check data cluster entries
	(each! (lambda (cluster_idx)
			(when-bind (entry (. exfat_obj :read_fat_entry cluster_idx))
				(cond
					; Free cluster
					((= entry +exfat_fat_free)
						(setq free_count (inc free_count)))

					; Bad cluster
					((= entry +exfat_fat_bad)
						(print "  WARNING: Bad cluster found at " cluster_idx)
						(setq warnings (inc warnings)))

					; End of chain
					((= entry +exfat_fat_eoc)
						(push allocated_clusters cluster_idx))

					; Points to another cluster
					((<= 2 entry (+ cluster_count 1))
						(push allocated_clusters cluster_idx))

					; Invalid entry
					(:t
						(print "  ERROR: Invalid FAT entry at cluster " cluster_idx ": 0x" (str-from-num entry 16))
						(setq errors (inc errors))))))
		(+ cluster_count 2) :nil 2)

	(print "  Free clusters: " free_count " / " cluster_count)
	(print "  Allocated clusters: " (length allocated_clusters))

	(if (= errors 0)
		(print "  FAT table: OK (" warnings " warnings)")
		(print "  FAT table: " errors " errors, " warnings " warnings"))

	(list errors warnings))

(defun check_cluster_chains (exfat_obj)
	; Check for circular references and orphaned chains
	; Returns: list of (error_count warning_count)
	(defq errors 0
		warnings 0
		cluster_count (get exfat_obj :cluster_count)
		+exfat_fat_free 0x00000000
		+exfat_fat_eoc 0xFFFFFFFF
		visited (list)
		chains 0)

	(print "Checking cluster chains...")

	; Start from root directory cluster (cluster 2)
	(defun check_chain (start_cluster max_depth)
		(defq current_cluster start_cluster
			depth 0
			chain_valid :t)

		(while (and chain_valid (< depth max_depth))
			; Check if we've visited this cluster already
			(when (find visited current_cluster)
				(print "  ERROR: Circular reference detected at cluster " current_cluster)
				(setq errors (inc errors))
				(setq chain_valid :nil))

			(when chain_valid
				(push visited current_cluster)

				; Get next cluster in chain
				(when-bind (next_entry (. exfat_obj :read_fat_entry current_cluster))
					(cond
						; End of chain
						((= next_entry +exfat_fat_eoc)
							(setq chain_valid :nil))

						; Valid cluster reference
						((and (>= next_entry 2) (< next_entry (+ cluster_count 2)))
							(setq current_cluster next_entry
								depth (inc depth)))

						; Invalid reference
						(:t
							(print "  ERROR: Invalid cluster reference: " current_cluster " -> " next_entry)
							(setq errors (inc errors))
							(setq chain_valid :nil))))))

		depth)

	; Check root directory chain
	(defq root_cluster (get exfat_obj :root_dir_cluster))
	(when root_cluster
		(defq root_chain_length (check_chain root_cluster cluster_count))
		(print "  Root directory chain length: " root_chain_length)
		(setq chains (inc chains)))

	(if (= errors 0)
		(print "  Cluster chains: OK (" chains " chains checked)")
		(print "  Cluster chains: " errors " errors"))

	(list errors warnings))

(defun check_filesystem_size (exfat_obj)
	; Verify filesystem size calculations
	; Returns: list of (error_count warning_count)
	(defq errors 0
		warnings 0
		sector_size (get exfat_obj :sector_size)
		cluster_size (get exfat_obj :cluster_size)
		cluster_count (get exfat_obj :cluster_count)
		cluster_heap_offset (get exfat_obj :cluster_heap_offset)
		calculated_size (+ (* cluster_heap_offset sector_size)
			(* cluster_count cluster_size))
		reported_size (. exfat_obj :get_size))

	(print "Checking filesystem size...")
	(print "  Calculated size: " calculated_size " bytes")
	(print "  Reported size: " reported_size " bytes")

	(unless (= calculated_size reported_size)
		(print "  ERROR: Size mismatch")
		(setq errors (inc errors)))

	(if (= errors 0)
		(print "  Filesystem size: OK")
		(print "  Filesystem size: " errors " errors"))

	(list errors warnings))

(defun main ()
	(print "ExFat Filesystem Checker (fsck.exfat)")
	(print "=====================================")
	(print)

	; Check if filesystem path provided
	(print "Note: This checker validates an ExFat filesystem")
	(print "in a memory-stream. For file-based images, import first.")
	(print)

	; Create a test filesystem for demonstration
	(print "Creating test filesystem...")
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 10 1024 1024))

	; Format it
	(. exfat_obj :format fs_size)
	(print "Test filesystem created (10 MB)")
	(print)

	; Run checks
	(defq total_errors 0
		total_warnings 0)

	; Check 1: Boot sector
	(bind (list errors warnings) (check_boot_sector exfat_obj))
	(setq total_errors (+ total_errors errors)
		total_warnings (+ total_warnings warnings))
	(print)

	; Check 2: FAT entries
	(bind (list errors warnings) (check_fat_entries exfat_obj))
	(setq total_errors (+ total_errors errors)
		total_warnings (+ total_warnings warnings))
	(print)

	; Check 3: Cluster chains
	(bind (list errors warnings) (check_cluster_chains exfat_obj))
	(setq total_errors (+ total_errors errors)
		total_warnings (+ total_warnings warnings))
	(print)

	; Check 4: Filesystem size
	(bind (list errors warnings) (check_filesystem_size exfat_obj))
	(setq total_errors (+ total_errors errors)
		total_warnings (+ total_warnings warnings))
	(print)

	; Summary
	(print "Filesystem check complete")
	(print "=========================")
	(print "Total errors: " total_errors)
	(print "Total warnings: " total_warnings)
	(print)

	(if (= total_errors 0)
		(print "Filesystem is CLEAN")
		(print "Filesystem has ERRORS - repair recommended"))

	; Exit with error code
	total_errors)

; Run the checker
(main)
