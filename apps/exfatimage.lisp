;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ExFat Filesystem Image Tool
; Export/import filesystem images to/from files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")

(defun export_image (exfat_obj output_path)
	; Export filesystem image to a file
	; inputs
	; exfat_obj = ExFat filesystem instance
	; output_path = path to output file
	; outputs
	; :t if successful, :nil otherwise
	(print "Exporting filesystem image...")
	(print "  Output: " output_path)

	(defq stream_obj (get exfat_obj :stream)
		total_size (. exfat_obj :get_size)
		chunk_size (* 64 1024))  ; Read 64KB at a time

	(print "  Size: " total_size " bytes")

	; Open output file
	(when-bind (output_file (file-stream output_path file_open_write))
		(print "  Writing data...")

		; Seek to beginning of source stream
		(stream-seek stream_obj 0 0)

		; Copy data in chunks
		(defq bytes_written 0
			success :t)

		(while (and success (< bytes_written total_size))
			(defq remaining (- total_size bytes_written)
				read_size (if (< remaining chunk_size) remaining chunk_size))

			; Read chunk from memory stream
			(when-bind (chunk (read-blk stream_obj read_size))
				; Write chunk to file
				(if (write-blk output_file chunk)
					(setq bytes_written (+ bytes_written (length chunk)))
					(setq success :nil)))

			(unless success
				(print "  ERROR: Write failed at byte " bytes_written)))

		; Flush and close
		(stream-flush output_file)
		(close output_file)

		(if success
			(progn
				(print "  Export complete: " bytes_written " bytes written")
				:t)
			(progn
				(print "  Export FAILED")
				:nil))))

(defun import_image (input_path)
	; Import filesystem image from a file
	; inputs
	; input_path = path to input file
	; outputs
	; ExFat object if successful, :nil otherwise
	(print "Importing filesystem image...")
	(print "  Input: " input_path)

	; Open input file
	(when-bind (input_file (file-stream input_path file_open_read))
		; Determine file size
		(stream-seek input_file 0 2)  ; Seek to end
		(defq file_size (stream-seek input_file 0 1)  ; Get position
			chunk_size (* 64 1024))  ; Read 64KB at a time

		(print "  Size: " file_size " bytes")

		; Seek back to beginning
		(stream-seek input_file 0 0)

		; Create memory stream for filesystem
		(defq mem_stream (memory-stream)
			bytes_read 0
			success :t)

		(print "  Reading data...")

		; Copy data in chunks
		(while (and success (< bytes_read file_size))
			(defq remaining (- file_size bytes_read)
				read_size (if (< remaining chunk_size) remaining chunk_size))

			; Read chunk from file
			(when-bind (chunk (read-blk input_file read_size))
				; Write chunk to memory stream
				(if (write-blk mem_stream chunk)
					(setq bytes_read (+ bytes_read (length chunk)))
					(setq success :nil)))

			(unless success
				(print "  ERROR: Read failed at byte " bytes_read)))

		; Close input file
		(close input_file)

		(when success
			(print "  Import complete: " bytes_read " bytes read")

			; Create ExFat object from imported stream
			(defq exfat_obj (ExFat mem_stream))

			; Try to mount it
			(print "  Mounting filesystem...")
			(if (. exfat_obj :mount)
				(progn
					(print "  Mount successful!")
					exfat_obj)
				(progn
					(print "  Mount FAILED - image may be corrupted")
					:nil)))))

(defun clone_image (source_exfat dest_stream)
	; Clone filesystem to a new stream
	; inputs
	; source_exfat = source ExFat filesystem
	; dest_stream = destination stream
	; outputs
	; new ExFat object if successful, :nil otherwise
	(print "Cloning filesystem...")

	(defq source_stream (get source_exfat :stream)
		total_size (. source_exfat :get_size)
		chunk_size (* 64 1024)
		bytes_copied 0
		success :t)

	(print "  Size: " total_size " bytes")

	; Seek to beginning of source
	(stream-seek source_stream 0 0)

	; Copy data
	(while (and success (< bytes_copied total_size))
		(defq remaining (- total_size bytes_copied)
			read_size (if (< remaining chunk_size) remaining chunk_size))

		(when-bind (chunk (read-blk source_stream read_size))
			(if (write-blk dest_stream chunk)
				(setq bytes_copied (+ bytes_copied (length chunk)))
				(setq success :nil))))

	(when success
		(print "  Clone complete: " bytes_copied " bytes copied")
		(stream-flush dest_stream)

		; Create new ExFat object
		(defq dest_exfat (ExFat dest_stream))
		(. dest_exfat :mount)
		dest_exfat))

(defun compare_images (exfat_obj1 exfat_obj2)
	; Compare two filesystem images
	; outputs
	; :t if identical, :nil if different
	(print "Comparing filesystems...")

	(defq size1 (. exfat_obj1 :get_size)
		size2 (. exfat_obj2 :get_size))

	(if (not (= size1 size2))
		(progn
			(print "  Different sizes: " size1 " vs " size2)
			:nil)
		(progn
			(print "  Both are " size1 " bytes")

			(defq stream1 (get exfat_obj1 :stream)
				stream2 (get exfat_obj2 :stream)
				chunk_size (* 64 1024)
				bytes_compared 0
				identical :t)

			; Seek to beginning
			(stream-seek stream1 0 0)
			(stream-seek stream2 0 0)

			; Compare chunks
			(while (and identical (< bytes_compared size1))
				(defq remaining (- size1 bytes_compared)
					read_size (if (< remaining chunk_size) remaining chunk_size)
					chunk1 (read-blk stream1 read_size)
					chunk2 (read-blk stream2 read_size))

				(if (and chunk1 chunk2 (= chunk1 chunk2))
					(setq bytes_compared (+ bytes_compared read_size))
					(progn
						(print "  Difference found at byte " bytes_compared)
						(setq identical :nil))))

			(when identical
				(print "  Filesystems are IDENTICAL"))

			identical)))

(defun main ()
	(print "ExFat Filesystem Image Tool")
	(print "===========================")
	(print)

	; Demonstrate export functionality
	(print "DEMO 1: Creating and exporting a filesystem")
	(print "--------------------------------------------")

	; Create test filesystem
	(defq fs_stream (memory-stream)
		exfat_obj (ExFat fs_stream)
		fs_size (* 5 1024 1024))

	(. exfat_obj :format fs_size)
	(print "Created 5 MB filesystem")

	; Allocate some clusters
	(. exfat_obj :allocate_cluster)
	(. exfat_obj :allocate_cluster)
	(print "Allocated 2 clusters")
	(print)

	; Export would work like this (but file-stream requires real filesystem):
	(print "To export to file:")
	(print "  (export_image exfat_obj \"/path/to/image.exfat\")")
	(print)

	; Demonstrate cloning
	(print "DEMO 2: Cloning a filesystem")
	(print "-----------------------------")

	(defq clone_stream (memory-stream))
	(when-bind (clone_obj (clone_image exfat_obj clone_stream))
		(print "Filesystem cloned successfully")
		(print)

		; Compare original and clone
		(print "DEMO 3: Comparing filesystems")
		(print "------------------------------")
		(compare_images exfat_obj clone_obj))

	(print)
	(print "Image tool demonstration complete")
	(print)
	(print "Usage in real applications:")
	(print "  ; Export to file")
	(print "  (export_image my_exfat \"/path/to/backup.img\")")
	(print)
	(print "  ; Import from file")
	(print "  (defq restored_exfat (import_image \"/path/to/backup.img\"))")
	(print)
	(print "  ; Clone to new stream")
	(print "  (defq new_stream (memory-stream))")
	(print "  (defq cloned_exfat (clone_image original_exfat new_stream))")

	0)

; Run the demo
(main)
