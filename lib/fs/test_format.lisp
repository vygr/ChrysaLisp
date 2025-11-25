(import "lib/fs/exfat.inc")

(defun test-format ()
	(prin "Creating 4MB exFat image...")(print)
	
	;; Create memory stream
	(defq mstream (memory-stream))
	
	;; Create ExFat instance
	(defq exfat (ExFat))
	
	;; Format the volume
	(. exfat :format mstream 4)
	
	(prin "Format complete. Writing to test_exfat.img...")(print)
	
	;; Calculate full size
	(defq full_size (* 4 1048576))
	
	;; Write to file - need to write full 4MB
	(defq fstream (file-stream "test_exfat.img" +file_open_write))
	
	;; Seek to beginning of memory stream and read what we have
	(stream-seek mstream 0 0)
	
	;; Write the formatted data we have
	(defq chunk_size 4096)
	(defq written 0)
	(while (< written full_size)
		(stream-seek mstream written 0)
		(defq chunk (read-blk mstream chunk_size))
		(when chunk
			(write-blk fstream chunk)
			(setq written (+ written (length chunk))))
		(unless chunk
			;; Fill rest with zeros
			(write-blk fstream (pad "" (min chunk_size (- full_size written)) (ascii-char 0)))
			(setq written (+ written chunk_size))))
	
	(stream-flush fstream)
	
	(prin "Image written. Size: " full_size " bytes (")(prin (>> full_size 20))(prin " MB)")(print)
	(prin "Please mount with: hdiutil attach test_exfat.img")(print)
	(prin "Then verify volume label with: diskutil info /Volumes/CHRYSALISP")(print)
)

(catch
	(test-format)
	(progn (prin "Test failed with error: " _)(print) :t)
)

((ffi "service/gui/lisp_deinit"))
