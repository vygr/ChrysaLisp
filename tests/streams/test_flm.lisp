(report-header "Streams: flm, a film written a frame at a time")

(import "gui/lisp.inc")
(import "lib/streams/flm.inc")

(defun flm-frame (n)
	;a small frame, a box that moves and changes colour with its number
	(defq canvas (Canvas 48 32 1))
	(.-> canvas (:fill 0xff203040)
		(:set_color (+ 0xff000000 (* n 0x101820))) (:fbox (* n 5) (+ 2 n) 12 9)
		(:set_color 0xffffffff) (:plot (- 47 n) 30)))

(defun flm-pixels (canvas)
	;the pixels of a canvas, by way of a copy so that the canvas, which
	;may be a film part way through, is not disturbed
	(defq copy (Canvas 48 32 1) stream (memory-stream))
	(. copy :resize canvas)
	(pixmap-write (getf copy +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(read-blk stream 100000))

(each (lambda (format)
	;six frames pumped in, the film comes out of the stream
	(defq out (memory-stream) film (flm-open out format)
		want (map (# (flm-pixels (flm-frame %0))) (range 0 6)))
	(each (# (flm-add film (flm-frame %0))) (range 0 6))
	(assert-eq (cat "frames written, " (str format) " bit") 6 (flm-close film))
	;and played back, a frame at a time
	(stream-seek out 0 0)
	(defq size (stream-seek out 0 2) played (CPM-load (progn (stream-seek out 0 0) out)))
	(assert-true (cat "a film to play, " (str format) " bit") played)
	(when played
		(setf (getf played +canvas_pixmap 0) +pixmap_stream out 0)
		;every frame comes back to the bit, the first, and those after it,
		;which are decoded a run at a time and premultiplied as they are
		(defq same 0)
		(each (lambda (frame)
			(if (eql (flm-pixels played) frame) (setq same (inc same)))
			(. played :next_frame)) want)
		(assert-eq (cat "every frame played back to the bit, " (str format) " bit") 6 same)
		;and two frames that differ are told apart
		(assert-true "frames that differ are told apart"
			(not (eql (first want) (second want))))))
	'(32 24))
