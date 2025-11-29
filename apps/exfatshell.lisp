;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Interactive Filesystem Shell
; Explore and inspect ExFat filesystems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defq shell_running :t
	current_exfat :nil
	current_cluster 2)  ; Start at root cluster

(defun print_help ()
	; Print command help
	(print)
	(print "ExFat Shell Commands")
	(print "====================")
	(print "  help               - Show this help message")
	(print "  info               - Show filesystem information")
	(print "  stat               - Show filesystem statistics")
	(print "  cluster <num>      - Show cluster information")
	(print "  goto <num>         - Set current cluster")
	(print "  read [num]         - Read and display cluster data")
	(print "  write <data>       - Write data to current cluster")
	(print "  fat <num>          - Show FAT entry for cluster")
	(print "  chain [num]        - Show complete FAT chain")
	(print "  alloc              - Allocate a new cluster")
	(print "  free <num>         - Free a cluster")
	(print "  dump [num]         - Hex dump cluster")
	(print "  boot               - Show boot sector info")
	(print "  map                - Show cluster allocation map")
	(print "  search <hex>       - Search for hex pattern in clusters")
	(print "  exit, quit         - Exit shell")
	(print))

(defun cmd_info ()
	; Show filesystem information
	(when current_exfat
		(print)
		(print "Filesystem Information")
		(print "======================")
		(print "  Sector size:        " (get current_exfat :sector_size) " bytes")
		(print "  Cluster size:       " (get current_exfat :cluster_size) " bytes")
		(print "  FAT offset:         " (get current_exfat :fat_offset) " sectors")
		(print "  FAT length:         " (get current_exfat :fat_length) " sectors")
		(print "  Cluster heap:       " (get current_exfat :cluster_heap_offset) " sectors")
		(print "  Cluster count:      " (get current_exfat :cluster_count))
		(print "  Total size:         " (. current_exfat :get_size) " bytes")
		(print "  Current cluster:    " current_cluster)
		(print)))

(defun cmd_stat ()
	; Show filesystem statistics
	(when current_exfat
		(defq total_clusters (get current_exfat :cluster_count)
			allocated 0
			free 0
			bad 0
			i 2)

		; Count cluster states
		(while (< i (+ total_clusters 2))
			(when-bind (entry (. current_exfat :read_fat_entry i))
				(cond
					((= entry 0x00000000) (setq free (inc free)))
					((= entry 0xFFFFFFF7) (setq bad (inc bad)))
					(:t (setq allocated (inc allocated)))))
			(setq i (inc i)))

		(print)
		(print "Filesystem Statistics")
		(print "=====================")
		(print "  Total clusters:     " total_clusters)
		(print "  Allocated:          " allocated)
		(print "  Free:               " free)
		(print "  Bad:                " bad)
		(print "  Usage:              " (/ (* allocated 100) total_clusters) "%")
		(print)))

(defun cmd_cluster (cluster_num)
	; Show cluster information
	(when current_exfat
		(when (and (>= cluster_num 2) (< cluster_num (+ (get current_exfat :cluster_count) 2)))
			(print)
			(print "Cluster " cluster_num " Information")
			(print "====================")

			(when-bind (entry (. current_exfat :read_fat_entry cluster_num))
				(print "  FAT entry:          0x" (str-from-num entry 16 8))
				(print "  Status:             "
					(cond
						((= entry 0x00000000) "Free")
						((= entry 0xFFFFFFF7) "Bad Cluster")
						((= entry 0xFFFFFFFF) "End of Chain")
						((and (>= entry 2) (< entry 0xFFFFFFF7))
							(cat "Points to cluster " (str-from-num entry 10)))
						(:t "Reserved")))
				(print "  Offset:             " (* (- cluster_num 2) (get current_exfat :cluster_size)) " bytes from heap")
				(print)))))

(defun cmd_goto (cluster_num)
	; Set current cluster
	(when current_exfat
		(when (and (>= cluster_num 2) (< cluster_num (+ (get current_exfat :cluster_count) 2)))
			(setq current_cluster cluster_num)
			(print "Current cluster set to " cluster_num)
			(cmd_cluster cluster_num))))

(defun cmd_read (cluster_num)
	; Read and display cluster data
	(when current_exfat
		(defq target (if cluster_num cluster_num current_cluster))
		(when (and (>= target 2) (< target (+ (get current_exfat :cluster_count) 2)))
			(when-bind (data (. current_exfat :read_cluster target))
				(print)
				(print "Cluster " target " Data (first 256 bytes)")
				(print "===================")

				; Show first 256 bytes or less
				(defq display_len (if (> (length data) 256) 256 (length data))
					printable "")

				; Extract printable portion
				(defq i 0)
				(while (< i display_len)
					(defq byte_val (code data 1 i))
					(if (and (>= byte_val 32) (<= byte_val 126))
						(setq printable (cat printable (char byte_val)))
						(setq printable (cat printable ".")))
					(setq i (inc i)))

				(print printable)
				(print)
				(print "Total length: " (length data) " bytes")
				(print)))))

(defun cmd_write (data_str)
	; Write data to current cluster
	(when current_exfat
		(when (and (>= current_cluster 2) (< current_cluster (+ (get current_exfat :cluster_count) 2)))
			(if (. current_exfat :write_cluster current_cluster data_str)
				(print "Wrote " (length data_str) " bytes to cluster " current_cluster)
				(print "Write failed")))))

(defun cmd_fat (cluster_num)
	; Show FAT entry for cluster
	(when current_exfat
		(when (and (>= cluster_num 2) (< cluster_num (+ (get current_exfat :cluster_count) 2)))
			(when-bind (entry (. current_exfat :read_fat_entry cluster_num))
				(print "Cluster " cluster_num " FAT entry: 0x" (str-from-num entry 16 8))))))

(defun cmd_chain (cluster_num)
	; Show complete FAT chain
	(when current_exfat
		(defq start (if cluster_num cluster_num current_cluster)
			chain (list)
			current start
			max_depth 1000)

		(print)
		(print "FAT Chain starting from cluster " start)
		(print "=====================================")

		(while (and (> max_depth 0) (>= current 2) (< current 0xFFFFFFF7))
			(push chain current)
			(when-bind (entry (. current_exfat :read_fat_entry current))
				(if (= entry 0xFFFFFFFF)
					(progn
						(print "  " current " -> EOC")
						(setq current 0))  ; End loop
					(progn
						(print "  " current " -> " entry)
						(setq current entry))))
			(setq max_depth (dec max_depth)))

		(print)
		(print "Chain length: " (length chain) " clusters")
		(print "Total size:   " (* (length chain) (get current_exfat :cluster_size)) " bytes")
		(print)))

(defun cmd_alloc ()
	; Allocate a new cluster
	(when current_exfat
		(when-bind (cluster (. current_exfat :allocate_cluster))
			(print "Allocated cluster " cluster)
			(setq current_cluster cluster)
			(cmd_cluster cluster))))

(defun cmd_free (cluster_num)
	; Free a cluster
	(when current_exfat
		(when (and (>= cluster_num 2) (< cluster_num (+ (get current_exfat :cluster_count) 2)))
			(if (. current_exfat :free_cluster cluster_num)
				(print "Freed cluster " cluster_num)
				(print "Free failed")))))

(defun cmd_dump (cluster_num)
	; Hex dump cluster
	(when current_exfat
		(defq target (if cluster_num cluster_num current_cluster))
		(when (and (>= target 2) (< target (+ (get current_exfat :cluster_count) 2)))
			(when-bind (data (. current_exfat :read_cluster target))
				(print)
				(print "Hex Dump of Cluster " target)
				(print "=====================")

				; Show first 256 bytes in hex
				(defq display_len (if (> (length data) 256) 256 (length data))
					i 0)

				(while (< i display_len)
					; Print offset
					(when (= (logand i 15) 0)
						(print (str-from-num i 16 4) ":  "))

					; Print hex byte
					(defq byte_val (code data 1 i)
						hex_chars "0123456789ABCDEF"
						high (>> byte_val 4)
						low (logand byte_val 0x0F))

					(print (char (code hex_chars 1 high))
						(char (code hex_chars 1 low)) " ")

					; Print ASCII at end of line
					(when (= (logand i 15) 15)
						(print " |")
						(defq j (- i 15))
						(while (<= j i)
							(defq ch (code data 1 j))
							(if (and (>= ch 32) (<= ch 126))
								(print (char ch))
								(print "."))
							(setq j (inc j)))
						(print "|")
						(print))

					(setq i (inc i)))

				(print)
				(print)))))

(defun cmd_boot ()
	; Show boot sector information
	(when current_exfat
		(defq stream (get current_exfat :stream)
			sector_size (get current_exfat :sector_size))

		(stream-seek stream 0 0)
		(when-bind (boot_sector (read-blk stream sector_size))
			(print)
			(print "Boot Sector Information")
			(print "=======================")

			; Jump boot
			(print "  Jump boot:          "
				(str-from-num (code boot_sector 1 0) 16 2) " "
				(str-from-num (code boot_sector 1 1) 16 2) " "
				(str-from-num (code boot_sector 1 2) 16 2))

			; Filesystem name
			(print "  Filesystem name:    " (slice boot_sector 3 11))

			; Signature
			(print "  Boot signature:     "
				(str-from-num (code boot_sector 1 510) 16 2) " "
				(str-from-num (code boot_sector 1 511) 16 2))
			(print))))

(defun cmd_map ()
	; Show cluster allocation map
	(when current_exfat
		(print)
		(print "Cluster Allocation Map")
		(print "======================")
		(print "Legend: . = free, # = allocated, B = bad, E = EOC")
		(print)

		(defq total_clusters (get current_exfat :cluster_count)
			i 2
			line "")

		(while (< i (+ total_clusters 2))
			(when-bind (entry (. current_exfat :read_fat_entry i))
				(defq symbol
					(cond
						((= entry 0x00000000) ".")
						((= entry 0xFFFFFFF7) "B")
						((= entry 0xFFFFFFFF) "E")
						(:t "#")))

				(setq line (cat line symbol))

				; Print line every 64 clusters
				(when (= (logand (- i 2) 63) 63)
					(print line)
					(setq line "")))

			(setq i (inc i)))

		; Print remaining
		(when (> (length line) 0)
			(print line))

		(print)))

(defun cmd_search (hex_str)
	; Search for hex pattern in clusters
	(when current_exfat
		(print "Searching for pattern: " hex_str)
		(print "Search not yet implemented")
		; TODO: Implement pattern search across clusters
		))

(defun parse_number (str)
	; Parse number from string (decimal or hex)
	; inputs: str = string like "42" or "0x2A"
	; outputs: number or :nil
	(when (> (length str) 0)
		(if (and (>= (length str) 2) (= (slice str 0 2) "0x"))
			; Hex number
			(str-to-num (slice str 2 (length str)) 16)
			; Decimal number
			(str-to-num str 10))))

(defun split_command (input)
	; Split input into command and arguments
	; inputs: input = command line string
	; outputs: list of (command arg1 arg2 ...)
	(defq parts (list)
		current ""
		i 0)

	(while (< i (length input))
		(defq ch (code input 1 i))
		(if (= ch 32)  ; Space
			(when (> (length current) 0)
				(push parts current)
				(setq current ""))
			(setq current (cat current (char ch))))
		(setq i (inc i)))

	; Add final part
	(when (> (length current) 0)
		(push parts current))

	parts)

(defun execute_command (input)
	; Execute a command
	; inputs: input = command line string
	(defq parts (split_command input))

	(when (> (length parts) 0)
		(defq cmd (get parts 0))

		(cond
			((or (= cmd "exit") (= cmd "quit"))
				(setq shell_running :nil)
				(print "Goodbye!"))

			((= cmd "help")
				(print_help))

			((= cmd "info")
				(cmd_info))

			((= cmd "stat")
				(cmd_stat))

			((= cmd "cluster")
				(if (> (length parts) 1)
					(when-bind (num (parse_number (get parts 1)))
						(cmd_cluster num))
					(print "Usage: cluster <number>")))

			((= cmd "goto")
				(if (> (length parts) 1)
					(when-bind (num (parse_number (get parts 1)))
						(cmd_goto num))
					(print "Usage: goto <number>")))

			((= cmd "read")
				(if (> (length parts) 1)
					(when-bind (num (parse_number (get parts 1)))
						(cmd_read num))
					(cmd_read :nil)))

			((= cmd "write")
				(if (> (length parts) 1)
					(cmd_write (get parts 1))
					(print "Usage: write <data>")))

			((= cmd "fat")
				(if (> (length parts) 1)
					(when-bind (num (parse_number (get parts 1)))
						(cmd_fat num))
					(print "Usage: fat <number>")))

			((= cmd "chain")
				(if (> (length parts) 1)
					(when-bind (num (parse_number (get parts 1)))
						(cmd_chain num))
					(cmd_chain :nil)))

			((= cmd "alloc")
				(cmd_alloc))

			((= cmd "free")
				(if (> (length parts) 1)
					(when-bind (num (parse_number (get parts 1)))
						(cmd_free num))
					(print "Usage: free <number>")))

			((= cmd "dump")
				(if (> (length parts) 1)
					(when-bind (num (parse_number (get parts 1)))
						(cmd_dump num))
					(cmd_dump :nil)))

			((= cmd "boot")
				(cmd_boot))

			((= cmd "map")
				(cmd_map))

			((= cmd "search")
				(if (> (length parts) 1)
					(cmd_search (get parts 1))
					(print "Usage: search <hex>")))

			(:t
				(print "Unknown command: " cmd)
				(print "Type 'help' for available commands")))))

(defun main ()
	(print "ExFat Interactive Filesystem Shell")
	(print "===================================")
	(print)

	; Create demo filesystem
	(print "Creating demo filesystem...")
	(defq fs_stream (memory-stream)
		fs_size (* 5 1024 1024))

	(setq current_exfat (ExFat fs_stream))
	(. current_exfat :format fs_size)

	(print "Demo filesystem created (5 MB)")
	(print "Type 'help' for available commands")
	(print)

	; Demo mode - execute some commands automatically
	(print "=== Running Demo Commands ===")
	(print)

	(execute_command "info")
	(execute_command "alloc")
	(execute_command "write Test-Data-In-Cluster")
	(execute_command "read")
	(execute_command "chain")
	(execute_command "map")

	(print)
	(print "=== Demo Complete ===")
	(print)
	(print "Shell ready. Type 'help' for commands, 'exit' to quit.")
	(print)

	0)

; Run the demo
(main)
