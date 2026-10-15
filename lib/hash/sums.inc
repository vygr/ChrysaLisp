;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sums, to see that data is what it was
;
; (crc32 data [crc]) -> crc
; (adler32 data [adler]) -> adler
;
; CRC-32, of PNG, zip and gzip, and Adler-32, of zlib, native. Each takes
; what the sum was before data, so a stream is summed a part at a time:
; 0 to begin a CRC-32 with, 1 an Adler-32.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(ffi (path-to-absolute "./crc32") sums-crc32)
(ffi (path-to-absolute "./adler32") sums-adler32)

(defun crc32 (data &optional crc)
	; (crc32 data [crc]) -> crc
	(sums-crc32 data (ifn crc 0)))

(defun adler32 (data &optional adler)
	; (adler32 data [adler]) -> adler
	(sums-adler32 data (ifn adler 1)))

;module
(export-symbols '(crc32 adler32))
(env-pop)
