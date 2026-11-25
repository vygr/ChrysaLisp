(report-header "System: TGA read, its id field skipped, and written")

(import "gui/canvas/lisp.inc")
(import "lib/image/tga.inc")

;a .tga of two pixels, 32 bits each, with an id field of the words given
;between the header and the pixels: its first byte is the id's length
(defun tt-file (id)
	(cat (char (length id)) (char 0) (char 2) (char 0 8) (char 0)
		(char 2 2) (char 1 2) (char 32) (char 0x28) id
		(char 0xff112233 4) (char 0xff445566 4)))

(defun tt-pixels (file)
	; (tt-pixels file) -> :nil | str of its pixels, blue green red alpha
	(when (defq canvas (TGA-load (string-stream file)))
		(defq out (string-stream (cat "")))
		(pixmap-write (getf canvas +canvas_pixmap 0) out 32)
		(str out)))

(assert-eq "the pixels of one with no id" (cat (char 0xff112233 4) (char 0xff445566 4)) (tt-pixels (tt-file "")))
(assert-eq "an id is not read as pixels" (tt-pixels (tt-file "")) (tt-pixels (tt-file "words of the picture")))
(assert-eq "the longest id, 255 bytes" (tt-pixels (tt-file "")) (tt-pixels (tt-file (pad "" 255 "x"))))
(assert-list-eq "its size and depth" '(2 1 32) (TGA-info (string-stream (tt-file "an id"))))

;written, and read back the same
(defq tt_out (memory-stream) tt_in (TGA-load (string-stream (tt-file "id"))))
(TGA-save tt_in tt_out 32)
(stream-seek tt_out 0 0)
(assert-eq "written and read again" (tt-pixels (tt-file "")) (progn
	(defq tt_back (TGA-load tt_out) tt_s (string-stream (cat "")))
	(pixmap-write (getf tt_back +canvas_pixmap 0) tt_s 32)
	(str tt_s)))
