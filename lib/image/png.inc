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

;module
(env-push)

(defq +png_signature (cat (char 0x89) "PNG" (char 13) (char 10) (char 26) (char 10)))

(defun png-be (data at)
	; (png-be data at) -> num
	;four bytes high first, as a .png has its numbers
	(+ (<< (code data 1 at) 24) (<< (code data 1 (+ at 1)) 16) (<< (code data 1 (+ at 2)) 8) (code data 1 (+ at 3))))

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
		prev (apply (const nums) (map (lambda (_) 0) (range 0 row_len)))
		most (dec (<< 1 depth)) row 0)
	(while (< row height)
		(task-slice)
		(defq kind (read-char in) raw (read-blk in row_len))
		(cond
			((or (not kind) (not raw) (< (length raw) row_len)) (setq row height))
			(:t (defq cur (apply (const nums) (map (# (code raw 1 %0)) (range 0 row_len))))
				;the filter undone, against the bytes of the pixel to the left,
				;those above, and those above the left
				(case kind
					(1 (each (# (elem-set cur %0 (logand (+ (elem-get cur %0) (elem-get cur (- %0 step))) 0xff)))
						(range step row_len)))
					(2 (each (# (elem-set cur %0 (logand (+ (elem-get cur %0) (elem-get prev %0)) 0xff)))
						(range 0 row_len)))
					(3 (each (# (elem-set cur %0 (logand (+ (elem-get cur %0)
							(>> (+ (if (>= %0 step) (elem-get cur (- %0 step)) 0) (elem-get prev %0)) 1)) 0xff)))
						(range 0 row_len)))
					(4 (each (lambda (i)
							(defq a (if (>= i step) (elem-get cur (- i step)) 0) b (elem-get prev i)
								c (if (>= i step) (elem-get prev (- i step)) 0)
								p (- (+ a b) c) pa (abs (- p a)) pb (abs (- p b)) pc (abs (- p c)))
							(elem-set cur i (logand (+ (elem-get cur i)
								(cond ((and (<= pa pb) (<= pa pc)) a) ((<= pb pc) b) (c))) 0xff)))
						(range 0 row_len))))
				;its pixels, blue green red alpha
				(defq pixels (cap (* width 4) (list)))
				(each (lambda (x)
					(defq a 255)
					(cond
						((< depth 8)
							;so many to a byte, the first in the high bits
							(defq at (* x depth) v (logand (>> (elem-get cur (>> at 3)) (- 8 depth (logand at 7))) most))
							(cond
								((= colour 3)
									(if (< v (length clear)) (setq a (code clear 1 v)))
									(setq v (* v 3))
									(push pixels (code palette 1 (+ v 2)) (code palette 1 (+ v 1)) (code palette 1 v) a))
								(:t (setq v (/ (* v 255) most))
									(push pixels v v v a))))
						(:t ;of 8 bits, or 16, the high byte of each
							(defq i (* x channels (/ depth 8)) w (/ depth 8))
							(case colour
								(0 (defq v (elem-get cur i)) (push pixels v v v 255))
								(2 (push pixels (elem-get cur (+ i w w)) (elem-get cur (+ i w)) (elem-get cur i) 255))
								(3 (defq v (elem-get cur i))
									(if (< v (length clear)) (setq a (code clear 1 v)))
									(setq v (* v 3))
									(push pixels (code palette 1 (+ v 2)) (code palette 1 (+ v 1)) (code palette 1 v) a))
								(4 (defq v (elem-get cur i)) (push pixels v v v (elem-get cur (+ i w))))
								(6 (push pixels (elem-get cur (+ i w w)) (elem-get cur (+ i w)) (elem-get cur i)
									(elem-get cur (+ i w w w))))))))
					(range 0 width))
				(write-blk out (apply (const cat) (cat (list "") (map (const char) pixels))))
				(setq prev cur row (inc row))))))

(defun png-stage (fn downstream_mbox src &rest args)
	;a stage of the pipeline, as a CPM's: what it reads is the stream if
	;it is first, or an in stream it says where to find
	(defq in_src (if (str? src)
		`(progn (mail-send (hex-decode ,(hex-encode src)) (in-mbox (defq in (in-stream)))) in)
		`(obj-ref ,(weak-ref src))))
	(str `(progn (import "lib/image/png.inc") (import "lib/streams/deflate.inc")
		(,fn ,in_src (out-stream (hex-decode ,(hex-encode downstream_mbox))) ~args))))

(defun png-stage-pixmap (pixmap handshake_mbox)
	;the last stage, the pixels read into the pixmap, and that said
	(str `(progn (import "gui/pixmap/lisp.inc")
		(mail-send (hex-decode ,(hex-encode handshake_mbox))
			(in-mbox (defq pixmap (obj-ref ,(weak-ref pixmap)) in (in-stream))))
		(mail-send (hex-decode ,(hex-encode handshake_mbox))
			(str (weak-ref (pixmap-read pixmap in 32)))))))

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
			(defq canvas (Canvas w h 1) pixmap (getf canvas +canvas_pixmap 0) handshake_mbox (mail-mbox))
			(setf pixmap +pixmap_type 32 0)
			;stream -> idat -> inflate -> unfilter -> pixmap, made from the end
			(open-child (png-stage-pixmap pixmap handshake_mbox) +kn_call_pin)
			(open-child (png-stage 'png-unfilter (mail-read handshake_mbox) handshake_mbox
				;the palette and what is clear are bytes, given as hex, a reader
				;takes no byte as it is in the text of a form
				w h depth colour (list 'hex-decode (hex-encode palette)) (list 'hex-decode (hex-encode clear))) +kn_call_pin)
			(open-child (png-stage 'inflate-decompress (mail-read handshake_mbox) handshake_mbox :t) +kn_call_pin)
			(open-child (png-stage 'png-idat (mail-read handshake_mbox) stream (second chunk)) +kn_call_pin)
			(if (eql (mail-read handshake_mbox) (const (str :nil))) (setq canvas :nil))))
	canvas)

;the canvas is told what this reads
(canvas-loader ".png" PNG-info PNG-load)

;module
(export-symbols '(PNG-load PNG-info png-idat png-unfilter))
(env-pop)
