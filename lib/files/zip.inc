;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .zip read
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././streams/deflate.inc")

;module
(env-push)

;what each thing in a zip is, from its directory
(enums +zip_entry 0
	(enum name method packed full at))

(defun zip-end (data)
	; (zip-end data) -> :nil | at
	;where the record that ends the zip is, looked for from the end back
	(defq at (- (length data) 22) least (max 0 (- (length data) 65557)) found :nil)
	(while (and (not found) (>= at least))
		(if (= (code data 4 at) 0x06054b50) (setq found at) (-- at)))
	found)

(defun zip-list (data)
	; (zip-list data) -> :nil | ((name method packed full at) ...)
	;what is in a zip, its directory. A folder is a name that ends in /.
	;:nil if it is not a zip
	(when (defq end (zip-end data))
		(defq count (code data 2 (+ end 10)) at (code data 4 (+ end 16)) out (list))
		(while (and (> count 0) (<= (+ at 46) (length data)) (= (code data 4 at) 0x02014b50))
			(defq name_len (code data 2 (+ at 28)) extra_len (code data 2 (+ at 30)) note_len (code data 2 (+ at 32)))
			(push out (list (slice data (+ at 46) (+ at 46 name_len))
				(code data 2 (+ at 10)) (code data 4 (+ at 20)) (code data 4 (+ at 24)) (code data 4 (+ at 42))))
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
	(bind '(name method packed full at) entry)
	(when (and (<= (+ at 30) (length data)) (= (code data 4 at) 0x04034b50))
		(defq from (+ at 30 (code data 2 (+ at 26)) (code data 2 (+ at 28))))
		(when (<= (+ from packed) (length data))
			(defq bytes (slice data from (+ from packed)))
			(case method
				(0 (write-blk out bytes) :t)
				(8 (inflate-decompress (string-stream bytes) out))))))

;module
(export-symbols '(zip-list zip-find zip-read
	+zip_entry_name +zip_entry_method +zip_entry_packed +zip_entry_full +zip_entry_at))
(env-pop)
