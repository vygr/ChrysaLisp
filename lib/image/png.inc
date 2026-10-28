;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; png loader
;
; A .png is a signature and then chunks, each a length, a type, that much
; data and a sum. IHDR first, how big and what kind of pixel, PLTE the
; palette if it has one, tRNS what of it is clear, and then the pixels, in
; one or more IDAT that together are one zlib stream. Each row of them,
; inflated, is a byte that says how it was filtered, against the pixel to
; its left, the one above, or both, and then the row.
;
; It is read as a CPM is, a pipeline of tasks on this node, a stream to a
; stream each, no frame held between them: the IDAT data taken out of the
; chunks, inflated, unfiltered into pixels of 32 bits, and read into the
; pixmap. What comes before the first IDAT is read here, it says what the
; stages are to be.
;
; Every kind of pixel PNG has is read, grey, colour, a palette, with alpha
; or not, of 1 to 16 bits. An interlaced one, Adam7, is not yet.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "././streams/stages.inc")
(import "././hash/sums.inc")

;module
(env-push)

(ffi (path-to-absolute "./png_unfilter") png-unfilter-row)
(ffi (path-to-absolute "./png_pixels") png-pixels)

(defq +png_signature (cat (char 0x89) "PNG" (char 13) (char 10) (char 26) (char 10)))

(defmacro png-be (data at)
	; (png-be data at) -> num
	;four bytes high first, as a .png has its numbers
	(static-qq (code-be ,data 4 ,at)))

(defun png-chunk (stream)
	; (png-chunk stream) -> :nil | (type data)
	;the next chunk, its sum read and not looked at
	(and (defq head (read-blk stream 8)) (= (length head) 8)
		(defq len (png-be head 0) data (if (> len 0) (read-blk stream len) ""))
		(= (length data) len) (read-blk stream 4)
		(list (slice head 4 8) data)))

(defun png-head (stream)
	; (png-head stream) -> :nil | (width height depth colour interlace)
	(and (eql (read-blk stream 8) +png_signature)
		(defq chunk (png-chunk stream)) (eql (first chunk) "IHDR") (>= (length (second chunk)) 13)
		(defq data (second chunk))
		(list (png-be data 0) (png-be data 4) (code data 1 8) (code data 1 9) (code data 1 12))))

(defun PNG-info (stream)
	; (PNG-info stream) -> (width height type) | (-1 -1 -1)
	(if (and stream (defq head (png-head stream)))
		(list (first head) (second head) 32)
		'(-1 -1 -1)))

(defun png-idat (in out len)
	; (png-idat in out len)
	;the data of the IDAT chunks, one after the other, the first of them
	;begun, len of it to come. It stops at a chunk that is not one
	(defq going :t)
	(while going
		(task-slice)
		(while (> len 0)
			(defq part (read-blk in (min len 65536)))
			(cond
				((or (not part) (= (length part) 0)) (setq len 0 going :nil))
				(:t (write-blk out part) (setq len (- len (length part))))))
		(when going
			;its sum, and the head of the next
			(read-blk in 4)
			(cond
				((and (defq head (read-blk in 8)) (= (length head) 8) (eql (slice head 4 8) "IDAT"))
					(setq len (png-be head 0)))
				((setq going :nil))))))

(defun png-channels (colour)
	; (png-channels colour) -> num
	(case colour (0 1) (2 3) (3 1) (4 2) (6 4) (:t 0)))

(defun png-unfilter (in out width height depth colour palette clear)
	; (png-unfilter in out width height depth colour palette clear)
	;the rows inflated, each its filter undone and its pixels written as
	;32 bits, blue green red alpha, as a pixmap reads them
	(defq channels (png-channels colour) bits (* channels depth)
		row_len (/ (+ (* width bits) 7) 8) step (max 1 (/ bits 8))
		prev (apply (const cat) (fill (list) row_len "\x00"))
		most (dec (<< 1 depth)) row 0)
	(while (< row height)
		(task-slice)
		(defq kind (read-char in) cur (read-blk in row_len))
		(cond
			((or (not kind) (not cur) (< (length cur) row_len)) (setq row height))
			(:t ;the filter undone, against the bytes of the pixel to the left,
				;those above, and those above the left, native
				(png-unfilter-row cur prev kind step)
				;its pixels, blue green red alpha, native for 8 bits or 16
				(cond
					((>= depth 8) (write-blk out (png-pixels cur colour depth width palette clear)))
					(:t (defq pixels (cap (* width 4) (list)))
						(each (lambda (x)
							;so many to a byte, the first in the high bits
							(defq a 255 at (* x depth) v (logand (>> (code cur 1 (>> at 3)) (- 8 depth (logand at 7))) most))
							(cond
								((= colour 3)
									(if (< v (length clear)) (setq a (code clear 1 v)))
									(setq v (* v 3))
									(if (< (+ v 2) (length palette))
										(push pixels (code palette 1 (+ v 2)) (code palette 1 (+ v 1)) (code palette 1 v) a)
										(push pixels 0 0 0 a)))
								(:t (setq v (/ (* v 255) most))
									(push pixels v v v a))))
							(range 0 width))
						(write-blk out (apply (const cat) (cat (list "") (map (const char) pixels))))))
				(setq prev cur row (inc row))))))

(defun png-chunk-head (stream)
	; (png-chunk-head stream) -> :nil | (type len)
	;the head of the next chunk, its data not read
	(and (defq head (read-blk stream 8)) (= (length head) 8)
		(list (slice head 4 8) (png-be head 0))))

(defun PNG-load (stream)
	; (PNG-load stream) -> :nil | canvas
	(defq canvas :nil)
	(when (and stream (defq head (png-head stream)))
		(bind '(w h depth colour interlace) head)
		;what comes before the pixels, read here
		(defq palette "" clear "" chunk :nil)
		(while (and (setq chunk (png-chunk-head stream)) (nql (first chunk) "IDAT"))
			(defq data (if (> (second chunk) 0) (read-blk stream (second chunk)) ""))
			(read-blk stream 4)
			(cond
				((eql (first chunk) "PLTE") (setq palette data))
				((eql (first chunk) "tRNS") (setq clear data))
				((eql (first chunk) "IEND") (setq chunk :nil))))
		(when (and chunk (= interlace 0) (> (png-channels colour) 0) (find depth '(1 2 4 8 16)))
			(defq canvas (Canvas w h 1) pixmap (getf canvas +canvas_pixmap 0))
			(setf pixmap +pixmap_type 32 0)
			;stream, idat, inflate, unfilter, pixmap. The palette and what is
			;clear are bytes, given as hex, a reader takes no byte as it is in
			;the text of a form
			(unless (stages-load stream pixmap 32 (list
					(stage "lib/image/png.inc" 'png-idat (second chunk))
					(stage "lib/streams/deflate.inc" 'inflate-decompress :t)
					(stage "lib/image/png.inc" 'png-unfilter w h depth colour
						(list 'hex-decode (hex-encode palette)) (list 'hex-decode (hex-encode clear)))))
				(setq canvas :nil))))
	canvas)

(defmacro png-be4 (v)
	;four bytes high first
	(static-qq (char-be ,v 4)))

(defun png-chunk-write (stream kind data)
	; (png-chunk-write stream kind data)
	;a chunk, its length, its kind, the data and the CRC-32 of the two
	(write-blk stream (png-be4 (length data)))
	(write-blk stream kind)
	(if (nempty? data) (write-blk stream data))
	(write-blk stream (png-be4 (crc32 data (crc32 kind)))))

(defun png-rows (in out width height)
	; (png-rows in out width height)
	;a stage, the rows of 32 bit pixels, blue green red alpha, as rows of a
	;PNG of colour 6, each its filter, 0, and red green blue alpha: the
	;swap png-pixels does of 8 bits is its own way back
	(defq row_len (* width 4) y 0)
	(while (and (< y height) (defq row (read-blk in row_len)) (= (length row) row_len))
		(task-slice)
		(write-char out 0)
		(write-blk out (png-pixels row 6 8 width "" ""))
		(++ y)))

(defun png-chunks (in out)
	; (png-chunks in out)
	;a stage, the zlib of the rows as IDAT chunks, then the IEND
	(while (and (defq data (read-blk in 65536)) (nempty? data))
		(png-chunk-write out "IDAT" data))
	(png-chunk-write out "IEND" "")
	(stream-flush out))

(defun PNG-save (canvas stream &rest _)
	; (PNG-save canvas stream) -> canvas
	;the canvas as a PNG of 8 bits of red, green, blue and alpha, the rows
	;filtered by none, a stage of a pipeline each as the load is: its rows,
	;deflate and the chunks
	(defq pixmap (getf canvas +canvas_pixmap 0)
		w (getf pixmap +pixmap_width 0) h (getf pixmap +pixmap_height 0))
	(write-blk stream +png_signature)
	(png-chunk-write stream "IHDR" (cat (png-be4 w) (png-be4 h) (char 8) (char 6) (char 0) (char 0) (char 0)))
	(stages-save pixmap 32 stream (list
		(stage "lib/image/png.inc" 'png-rows w h)
		(stage "lib/streams/deflate.inc" 'deflate-compress :t)
		(stage "lib/image/png.inc" 'png-chunks)))
	canvas)

;the canvas is told what this reads and writes
(canvas-loader ".png" PNG-info PNG-load PNG-save)

;module
(export-symbols '(PNG-load PNG-info PNG-save png-idat png-unfilter png-rows png-chunks))
(env-pop)
