(report-header "Streams: flm, a film written a frame at a time, and read as a reel of packets")

(import "gui/lisp.inc")
(import "lib/media/flm.inc")

(defun flm-made (n)
	;a small frame, a box that moves and changes colour with its number
	(defq canvas (Canvas 48 32 1))
	(.-> canvas (:fill 0xff203040)
		(:set_color (+ 0xff000000 (* n 0x101820))) (:fbox (* n 5) (+ 2 n) 12 9)
		(:set_color 0xffffffff) (:plot (- 47 n) 30)))

(defun flm-pixels (canvas)
	;the pixels of a canvas, by way of a copy, premultiplied as a frame of
	;a film is once it has been drawn on
	(defq copy (Canvas 48 32 1) stream (memory-stream))
	(. copy :resize canvas)
	(pixmap-write (getf copy +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(read-blk stream 100000))

(defun flm-play (reel)
	;every frame of a reel, in order, as pixels
	(defq codec (flm-codec reel))
	(map (# (flm-pixels (flm-decode codec (flm-packet reel %0) %0))) (range 0 (. reel :find :frames))))

(each (lambda (format)
	;six frames pumped in, the film comes out of the stream
	(defq out (memory-stream) film (flm-open out format 25)
		want (map (# (flm-pixels (flm-made %0))) (range 0 6)))
	(each (# (flm-add film (flm-made %0))) (range 0 6))
	(assert-eq (cat "frames written, " (str format) " bit") 6 (flm-close film))
	;a reel of it, a packet a frame and the rate it was made at
	(defq reel (flm-reel out))
	(assert-true (cat "a reel of it, " (str format) " bit") (and reel (= (. reel :find :version) 3)))
	(assert-eq (cat "a packet a frame, " (str format) " bit") 6 (. reel :find :frames))
	(assert-eq "the rate it was made at" 25 (n2i (. reel :find :rate)))
	;every frame comes back to the bit, the first whole, those after it
	;each from the one before
	(assert-eq (cat "every frame played back to the bit, " (str format) " bit") 6
		(length (filter (const identity) (map (const eql) (flm-play reel) want))))
	;round again from the first, as a player that loops does
	(defq codec (flm-codec reel))
	(each (# (flm-decode codec (flm-packet reel %0) %0)) (range 0 4))
	(assert-eq "back to the first, it is the first again" (first want)
		(flm-pixels (flm-decode codec (flm-packet reel 0) 0)))
	(assert-true "frames that differ are told apart" (not (eql (first want) (second want))))
	;as a player has it, a frame by its number, on and back and on
	(defq codec (flm-codec reel))
	(assert-list-eq "frames by number, as a player asks" (map (# (elem-get want %0)) '(0 1 4 2 2 5 0))
		(map (# (flm-pixels (flm-frame codec %0))) '(0 1 4 2 2 5 0)))
	;the same film as an older version was: no packets, version 2. It is
	;made this version again, and plays the same
	(stream-seek out 0 0)
	(defq whole (read-blk out 10000000) old (cat (slice whole 0 8) (char 2 4) (slice whole 12 28)) at 28)
	(while (< at (length whole))
		(defq len (code whole 4 at))
		(setq old (cat old (slice whole (+ at 4) (+ at 4 len))) at (+ at 4 len)))
	(defq again (memory-stream))
	(assert-eq (cat "an older film is made this version, " (str format) " bit") 6
		(flm-convert (string-stream old) again 25))
	(assert-list-eq "and plays the same" want (flm-play (flm-reel again))))
	'(32 24))

;the reel layer finds the kind by the name
(assert-true "a film is a kind of reel" (some (# (eql (first %0) ".flm")) *reel_kinds*))
(assert-true "a film shows as a picture of its first frame"
	(canvas-load "apps/media/films/data/cradle.flm" +load_flag_noswap))
