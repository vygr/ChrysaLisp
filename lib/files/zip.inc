;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .zip read and write
;
; A .zip is its files one after the other, each with a head of its own, and
; at the end a directory of them: each name, how it was packed, how long it
; is packed and not, and where its head is. The directory is found from the
; record that ends the file, which is in its last 64KB.
;
; A zip is given whole, as a str, as a file loaded or a fetch gives it.
; What it holds is read packed as it was, stored or by DEFLATE, the only
; two a zip of models has, and written to a stream, through
; (inflate-decompress) when it was packed.
;
; A zip is written whole, (zip-write), its files given as strs: each packed
; by DEFLATE, or stored when that is no smaller, its CRC-32 and a time,
; then the directory. Not ZIP64: no more than 65535 things, nor 4GB.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././streams/deflate.inc")
(import "././hash/sums.inc")
(import "././date/date.inc")

;module
(env-push)

;what each thing in a zip is, from its directory
(enums +zip_entry 0
	(enum name method packed full at crc))

(defun zip-end (data)
	; (zip-end data) -> :nil | at
	;where the record that ends the zip is, looked for from the end back
	(defq at (- (length data) 22) least (max 0 (- (length data) 65557)) found :nil)
	(while (and (not found) (>= at least))
		(if (= (code data 4 at) 0x06054b50) (setq found at) (-- at)))
	found)

(defun zip-list (data)
	; (zip-list data) -> :nil | ((name method packed full at crc) ...)
	;what is in a zip, its directory. A folder is a name that ends in /.
	;:nil if it is not a zip
	(when (defq end (zip-end data))
		(defq count (code data 2 (+ end 10)) at (code data 4 (+ end 16)) out (list))
		(while (and (> count 0) (<= (+ at 46) (length data)) (= (code data 4 at) 0x02014b50))
			(defq name_len (code data 2 (+ at 28)) extra_len (code data 2 (+ at 30)) note_len (code data 2 (+ at 32)))
			(push out (list (slice data (+ at 46) (+ at 46 name_len))
				(code data 2 (+ at 10)) (code data 4 (+ at 20)) (code data 4 (+ at 24)) (code data 4 (+ at 42))
				(code data 4 (+ at 16))))
			(setq at (+ at 46 name_len extra_len note_len) count (dec count)))
		out))

(defun zip-find (entries name)
	; (zip-find entries name) -> :nil | entry
	;the one of that name, or the one whose name ends so, a file in a
	;folder of the zip given by its own name
	(or (some (# (if (eql (first %0) name) %0)) entries)
		(some (# (if (ends-with (cat "/" name) (first %0)) %0)) entries)))

(defun zip-read (data entry out)
	; (zip-read data entry out) -> :nil | :t
	;what one thing of a zip is, to a stream. :nil if it was packed some
	;other way, or is not all there
	(bind '(name method packed full at &ignore) entry)
	(when (and (<= (+ at 30) (length data)) (= (code data 4 at) 0x04034b50))
		(defq from (+ at 30 (code data 2 (+ at 26)) (code data 2 (+ at 28))))
		(when (<= (+ from packed) (length data))
			(defq bytes (slice data from (+ from packed)))
			(case method
				(0 (write-blk out bytes) :t)
				(8 (inflate-decompress (string-stream bytes) out))))))

(defun zip-write (out files &optional stamp)
	; (zip-write out files [stamp]) -> :nil | count
	;a zip of files, ((name data) ...), to a stream: each packed by DEFLATE,
	;or stored as it is when that is no smaller, a name that ends in / a
	;folder, then the directory and the record that ends it. stamp is the
	;time each is said to be of, (date), now if not given. :nil if there
	;are more than 65535 or it would be past 4GB, which want ZIP64
	(bind '(dos_time dos_date) (date-dos (ifn stamp (date))))
	(defq at 0 dir (list) ok (< (length files) 65535))
	(each (lambda ((name data))
			(when ok
				(defq crc (crc32 data) method 0 packed data)
				(unless (or (ends-with "/" name) (empty? data))
					(defq ps (string-stream (cat "")))
					(deflate-compress (string-stream data) ps)
					(if (< (length (setq ps (str ps))) (length data)) (setq method 8 packed ps)))
				(defq head (cat (char 20 2) (char 0 2) (char method 2) (char dos_time 2) (char dos_date 2)
						(char crc 4) (char (length packed) 4) (char (length data) 4)
						(char (length name) 2) (char 0 2)))
				(write-blk out (cat (char 0x04034b50 4) head name))
				(write-blk out packed)
				(push dir (cat (char 0x02014b50 4) (char 20 2) head (char 0 2) (char 0 2) (char 0 2)
					(char (if (ends-with "/" name) 0x10 0) 4) (char at 4) name))
				(setq at (+ at 30 (length name) (length packed)))
				(if (>= at 0xffffffff) (setq ok :nil))))
		files)
	(when ok
		(defq size (reduce (# (+ %0 (length %1))) dir 0))
		(each (# (write-blk out %0)) dir)
		(write-blk out (cat (char 0x06054b50 4) (char 0 2) (char 0 2) (char (length dir) 2) (char (length dir) 2)
			(char size 4) (char at 4) (char 0 2)))
		(length dir)))

;module
(export-symbols '(zip-list zip-find zip-read zip-write
	+zip_entry_name +zip_entry_method +zip_entry_packed +zip_entry_full +zip_entry_at +zip_entry_crc))
(env-pop)
