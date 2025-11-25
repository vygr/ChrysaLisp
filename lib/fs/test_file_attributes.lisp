(import "./exfat.inc")

(defun test-attributes ()
	(prin "Testing different file attributes...")(print)
	
	(defq mem_stream (memory-stream))
	(defq fs (ExFat))
	
	(. fs :format 64 mem_stream)
	(. fs :mount)
	
	(defq root_cluster (get :root_cluster fs))
	
	;; Create files with different attributes using create_entry directly
	(prin "Creating files with different attributes...")(print)
	
	;; File 1: No attributes (0x00)
	(defq cluster1 (. fs :alloc_cluster))
	(. fs :create_entry root_cluster "attr_00.txt" 0x00 cluster1 0)
	(prin "  Created attr_00.txt (attr=0x00 - NONE)")(print)
	
	;; File 2: READ_ONLY (0x01)
	(defq cluster2 (. fs :alloc_cluster))
	(. fs :create_entry root_cluster "attr_01.txt" 0x01 cluster2 0)
	(prin "  Created attr_01.txt (attr=0x01 - READ_ONLY)")(print)
	
	;; File 3: ARCHIVE (0x20) - current default
	(defq cluster3 (. fs :alloc_cluster))
	(. fs :create_entry root_cluster "attr_20.txt" 0x20 cluster3 0)
	(prin "  Created attr_20.txt (attr=0x20 - ARCHIVE - DEFAULT)")(print)
	
	;; File 4: ARCHIVE + READ_ONLY (0x21)
	(defq cluster4 (. fs :alloc_cluster))
	(. fs :create_entry root_cluster "attr_21.txt" 0x21 cluster4 0)
	(prin "  Created attr_21.txt (attr=0x21 - ARCHIVE+READ_ONLY)")(print)
	
	;; File 5: Just in case - try what MacOS uses for directories (0x10 - DIR flag)
	(defq cluster5 (. fs :alloc_cluster))
	(. fs :create_entry root_cluster "attr_10.txt" 0x10 cluster5 0)
	(prin "  Created attr_10.txt (attr=0x10 - DIR flag)")(print)
	
	;; Write to disk
	(stream-seek mem_stream 0 0)
	(defq file_stream (file-stream "exfat_attr_test.img" +file_open_write))
	(defq chunk_size 1024)
	(defq total_size (* 64 1048576))
	(defq written 0)
	(while (< written total_size)
		(defq to_write (min chunk_size (- total_size written)))
		(defq buffer (read-blk mem_stream to_write))
		(write-blk file_stream buffer)
		(setq written (+ written to_write))
	)
	(stream-flush file_stream)
	
	(print)
	(prin "Image created: exfat_attr_test.img")(print)
	(prin "Test with: hdiutil attach exfat_attr_test.img")(print)
)

(catch
	(test-attributes)
	(progn (prin "Failed: " _)(print) :t))

((ffi "service/gui/lisp_deinit"))
