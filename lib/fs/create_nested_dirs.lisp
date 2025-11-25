(import "./exfat.inc")

(defun create-img ()
	(prin "Creating exfat.img with multiple directories...")(print)
	
	(defq mem_stream (memory-stream))
	(defq fs (ExFat))
	
	(. fs :format 64 mem_stream)
	(. fs :mount)
	
	;; Create top-level directories
	(. fs :create_dir "/dir1")
	(prin "Created /dir1")(print)
	
	(. fs :create_dir "/dir2")
	(prin "Created /dir2")(print)
	
	;; Create nested directory
	(. fs :create_dir "/dir1/subdir")
	(prin "Created /dir1/subdir")(print)
	
	;; Create a file in nested directory for good measure
	(. fs :create_file "/dir1/subdir/test.txt")
	(defq fh (. fs :open_file "/dir1/subdir/test.txt" :write))
	(. fs :write_file fh "Nested file")
	(. fs :close_file fh)
	(prin "Created /dir1/subdir/test.txt")(print)
	
	(. fs :unmount)
	
	;; Write to disk
	(stream-seek mem_stream 0 0)
	(defq file_stream (file-stream "exfat.img" +file_open_write))
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
	(prin "Image created.")(print)
)

(catch
	(create-img)
	(progn (prin "Failed: " _)(print) :t))

((ffi "service/gui/lisp_deinit"))
