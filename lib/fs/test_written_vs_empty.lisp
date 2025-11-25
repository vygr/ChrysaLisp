(import "./exfat.inc")

(defun test-written-files ()
	(prin "Testing files with written content...")(print)
	
	(defq mem_stream (memory-stream))
	(defq fs (ExFat))
	
	(. fs :format 64 mem_stream)
	(. fs :mount)
	
	;; Create file using create_file + write + close (normal flow)
	(prin "Creating file1.txt via create_file+write+close...")(print)
	(. fs :create_file "/file1.txt")
	(defq fh (. fs :open_file "/file1.txt" :write))
	(. fs :write_file fh "Hello World")
	(. fs :close_file fh)
	(prin "  Created file1.txt with content")(print)
	
	;; Create file using create_entry with data_len=0 (no write)
	(defq root_cluster (get :root_cluster fs))
	(defq cluster2 (. fs :alloc_cluster))
	(. fs :create_entry root_cluster "file2.txt" +EXFAT_ATTR_ARCHIVE cluster2 0)
	(prin "  Created file2.txt without content (data_len=0)")(print)
	
	;; Write to disk
	(stream-seek mem_stream 0 0)
	(defq file_stream (file-stream "exfat_written_test.img" +file_open_write))
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
	(prin "Image created: exfat_written_test.img")(print)
)

(catch
	(test-written-files)
	(progn (prin "Failed: " _)(print) :t))

((ffi "service/gui/lisp_deinit"))
