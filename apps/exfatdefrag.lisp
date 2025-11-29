;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Defragmentation Analyzer
; Analyzes cluster allocation patterns and fragmentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defun count_fragments (exfat_obj first_cluster)
	; Count number of fragments in a cluster chain
	; A fragment is a break in sequential cluster allocation
	; Returns (fragment_count cluster_count)
	; inputs
	; exfat_obj = ExFat filesystem object
	; first_cluster = starting cluster number
	(cond
		((or (= first_cluster 0) (= first_cluster +exfat_fat_eoc))
			; No clusters allocated
			(list 0 0))
		(:t
			(defq current_cluster first_cluster
				prev_cluster first_cluster
				cluster_count 1
				fragment_count 1)  ; First cluster is always first fragment

			; Follow the cluster chain
			(while :t
				(defq next_cluster (. exfat_obj :read_fat_entry current_cluster))

				; Check for end of chain
				(when (or (= next_cluster +exfat_fat_eoc) (= next_cluster 0))
					(return (list fragment_count cluster_count)))

				; Check if sequential (next cluster = current + 1)
				(unless (= next_cluster (+ current_cluster 1))
					; Non-sequential, increment fragment count
					(setq fragment_count (inc fragment_count)))

				(setq prev_cluster current_cluster)
				(setq current_cluster next_cluster)
				(setq cluster_count (inc cluster_count))))))

(defun analyze_file_fragmentation (exfat_obj file_info)
	; Analyze fragmentation for a single file
	; Returns hash with :name, :size, :clusters, :fragments, :fragmented
	; inputs
	; exfat_obj = ExFat filesystem object
	; file_info = file info hash from :stat
	(defq first_cluster (get file_info :first_cluster)
		name (get file_info :name)
		size (get file_info :size)
		is_dir (get file_info :is_directory))

	(defq frag_info (count_fragments exfat_obj first_cluster)
		fragment_count (get frag_info 0)
		cluster_count (get frag_info 1))

	(hash
		:name name
		:size size
		:is_directory is_dir
		:clusters cluster_count
		:fragments fragment_count
		:fragmented (> fragment_count 1)))

(defun collect_all_files (exfat_obj path files_list)
	; Recursively collect all files and directories
	; inputs
	; exfat_obj = ExFat filesystem object
	; path = current directory path
	; files_list = list to accumulate file info hashes
	(defq entries (. exfat_obj :list path))

	(each (lambda (entry)
		(defq name (get entry :name)
			is_dir (get entry :is_directory)
			full_path (cat path (if (eql path "/") "" "/") name))

		; Add this entry to the list
		(push files_list (hash
			:path full_path
			:name name
			:size (get entry :size)
			:is_directory is_dir
			:first_cluster (get entry :first_cluster)))

		; Recurse into directories
		(when is_dir
			(collect_all_files exfat_obj full_path files_list)))
		entries)

	files_list)

(defun analyze_filesystem (exfat_obj)
	; Analyze entire filesystem for fragmentation
	; Returns hash with overall statistics
	(defq all_files (collect_all_files exfat_obj "/" (list)))

	(defq total_files 0
		total_dirs 0
		fragmented_files 0
		total_fragments 0
		total_clusters 0
		max_fragments 0
		max_fragmented_file "")

	; Analyze each file
	(defq file_details (list))
	(each (lambda (file_info)
		(defq frag_info (analyze_file_fragmentation exfat_obj file_info)
			fragments (get frag_info :fragments)
			clusters (get frag_info :clusters)
			is_dir (get frag_info :is_directory))

		; Update statistics
		(if is_dir
			(setq total_dirs (inc total_dirs))
			(setq total_files (inc total_files)))

		(setq total_fragments (+ total_fragments fragments))
		(setq total_clusters (+ total_clusters clusters))

		(when (get frag_info :fragmented)
			(setq fragmented_files (inc fragmented_files))
			(when (> fragments max_fragments)
				(setq max_fragments fragments)
				(setq max_fragmented_file (get file_info :path))))

		(push file_details frag_info))
		all_files)

	; Calculate percentages
	(defq total_items (+ total_files total_dirs)
		fragmentation_pct (if (> total_items 0)
			(/ (* fragmented_files 100) total_items)
			0)
		avg_fragments (if (> total_items 0)
			(/ total_fragments total_items)
			0))

	(hash
		:total_files total_files
		:total_directories total_dirs
		:fragmented_files fragmented_files
		:total_fragments total_fragments
		:total_clusters total_clusters
		:fragmentation_percentage fragmentation_pct
		:average_fragments avg_fragments
		:max_fragments max_fragments
		:max_fragmented_file max_fragmented_file
		:file_details file_details))

(defun print_fragmentation_report (stats)
	; Print detailed fragmentation report
	; inputs
	; stats = statistics hash from analyze_filesystem
	(print)
	(print "ExFat Fragmentation Analysis Report")
	(print "===================================")
	(print)
	(print "Overall Statistics:")
	(print "  Total Files:       " (get stats :total_files))
	(print "  Total Directories: " (get stats :total_directories))
	(print "  Fragmented Items:  " (get stats :fragmented_files))
	(print "  Fragmentation:     " (get stats :fragmentation_percentage) "%")
	(print)
	(print "Cluster Statistics:")
	(print "  Total Clusters:    " (get stats :total_clusters))
	(print "  Total Fragments:   " (get stats :total_fragments))
	(print "  Average Fragments: " (get stats :average_fragments))
	(print)

	(when (> (get stats :max_fragments) 0)
		(print "Most Fragmented File:")
		(print "  Path:      " (get stats :max_fragmented_file))
		(print "  Fragments: " (get stats :max_fragments))
		(print))

	; Print per-file details if there are fragmented files
	(defq fragmented_details (filter
		(lambda (file) (get file :fragmented))
		(get stats :file_details)))

	(when (> (length fragmented_details) 0)
		(print "Fragmented Files/Directories:")
		(print "------------------------------")
		(each (lambda (file)
			(print "  " (get file :name))
			(print "    Clusters:  " (get file :clusters))
			(print "    Fragments: " (get file :fragments))
			(print "    Size:      " (get file :size) " bytes"))
			fragmented_details)
		(print))

	(if (= (get stats :fragmented_files) 0)
		(print "Filesystem Health: EXCELLENT (no fragmentation)")
		(if (< (get stats :fragmentation_percentage) 10)
			(print "Filesystem Health: GOOD (minimal fragmentation)")
			(if (< (get stats :fragmentation_percentage) 30)
				(print "Filesystem Health: FAIR (moderate fragmentation)")
				(print "Filesystem Health: POOR (significant fragmentation)")))))

(defun print_usage ()
	; Print usage information
	(print "Usage: exfatdefrag.lisp <filesystem_size>")
	(print)
	(print "Analyzes fragmentation in an ExFat filesystem.")
	(print)
	(print "Arguments:")
	(print "  filesystem_size   Size in MB (e.g., 10 for 10 MB)")
	(print)
	(print "Examples:")
	(print "  exfatdefrag.lisp 10    # Analyze 10 MB filesystem")
	(print "  exfatdefrag.lisp 100   # Analyze 100 MB filesystem")
	(print)
	(print "The tool creates a test filesystem, populates it with")
	(print "sample files and directories, and analyzes fragmentation."))

(defun create_test_filesystem (size_mb)
	; Create a test filesystem with some fragmentation
	; inputs
	; size_mb = filesystem size in MB
	; outputs
	; ExFat object with test data
	(defq fs_stream (memory-stream)
		fs_size (* size_mb 1024 1024))

	(defq exfat_obj (ExFat fs_stream))
	(. exfat_obj :format fs_size)
	(. exfat_obj :mount)

	; Create test structure with potential fragmentation
	(. exfat_obj :mkdir "/projects")
	(. exfat_obj :mkdir "/projects/app1")
	(. exfat_obj :mkdir "/projects/app2")
	(. exfat_obj :mkdir "/docs")
	(. exfat_obj :mkdir "/temp")

	; Create files
	(. exfat_obj :create "/projects/app1/main.lisp")
	(. exfat_obj :create "/projects/app1/utils.lisp")
	(. exfat_obj :create "/projects/app2/server.lisp")
	(. exfat_obj :create "/docs/readme.txt")
	(. exfat_obj :create "/docs/guide.txt")
	(. exfat_obj :create "/temp/cache.dat")

	; Create and delete some files to create fragmentation
	(. exfat_obj :create "/temp/delete_me1.tmp")
	(. exfat_obj :create "/temp/delete_me2.tmp")
	(. exfat_obj :remove "/temp/delete_me1.tmp")

	; Create more files in gaps
	(. exfat_obj :create "/projects/app1/config.lisp")
	(. exfat_obj :create "/docs/changelog.txt")

	exfat_obj)

(defun main ()
	; Main entry point
	(defq args (get-env "args"))

	(when (or (not args) (< (length args) 1))
		(print_usage)
		(return 1))

	; Parse filesystem size
	(defq size_str (get args 0)
		size_mb (num-str size_str))

	(when (or (not size_mb) (<= size_mb 0))
		(print "Error: Invalid filesystem size: " size_str)
		(print_usage)
		(return 1))

	(print "Creating " size_mb " MB test filesystem...")
	(defq exfat_obj (create_test_filesystem size_mb))

	(print "Analyzing fragmentation...")
	(defq stats (analyze_filesystem exfat_obj))

	(print_fragmentation_report stats)

	0)

; Run if executed directly
(when (get-env "args")
	(exit (main)))
