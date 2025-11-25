(import "./exfat.inc")

(defun create-img ()
	(prin "Creating exfat.img with 2 files...")(print)
	
	;; Use memory-stream for all operations
	(defq mem_stream (memory-stream))
	(unless mem_stream (throw "Failed to create memory stream"))
	
	(defq fs (ExFat))
	(prin "Formatting...")(print)
	(. fs :format 64 mem_stream)
	(prin "Formatted.")(print)
	
	;; Mount to add content
	(. fs :mount)
	(prin "Mounted.")(print)
	
	;; Create FIRST file
	(. fs :create_file "/file1.txt")
	(defq fh (. fs :open_file "/file1.txt" :write))
	(unless fh (throw "Failed to open /file1.txt"))
	(. fs :write_file fh "This is file 1")
	(. fs :close_file fh)
	(prin "Created /file1.txt")(print)
	
	;; Create SECOND file
	(. fs :create_file "/file2.txt")
	(setq fh (. fs :open_file "/file2.txt" :write))
	(unless fh (throw "Failed to open /file2.txt"))
	(. fs :write_file fh "This is file 2")
	(. fs :close_file fh)
	(prin "Created /file2.txt")(print)
	
	(. fs :unmount)
	(prin "Unmounted.")(print)
	
	;; Export to file using 1KB chunks
	(prin "Writing to disk...")(print)
	(stream-seek mem_stream 0 0)
	(defq file_stream (file-stream "exfat.img" +file_open_write))
	(unless file_stream (throw "Failed to create output file"))
	
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
	(prin "Written.")(print)
	
	(prin "Image 'exfat.img' created successfully.")(print)
)

(catch
	(create-img)
	(progn (prin "Test Failed with exception: " _)(print) :t))

;clean exit
((ffi "service/gui/lisp_deinit"))
