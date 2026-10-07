(report-header "System: a pixmap with its pixels in shared memory")

(import "gui/lisp.inc")

(defun px-bytes (pixmap)
	;the pixels of a pixmap, as they are saved
	(defq stream (memory-stream))
	(pixmap-write pixmap stream 32)
	(stream-seek stream 0 0)
	(read-blk stream 1000000))

(defq made (pixmap-shared 64 32 0) key (pixmap-key made) found (pixmap-shared 64 32 key))
(assert-true "made" made)
(assert-true "it has a key" (/= key 0))
(assert-true "found by its key" found)
(assert-eq "the one that found it has the key" key (pixmap-key found))
(assert-eq "one that was never made is not found" :nil (pixmap-shared 64 32 (+ key 1)))
(assert-eq "one that is smaller than asked for is not found" :nil (pixmap-shared 640 320 key))
(assert-true "two keys differ" (/= key (pixmap-key (pixmap-shared 64 32 0))))
(assert-eq "a plain pixmap has no key" 0 (pixmap-key (getf (Canvas 8 8 1) +canvas_pixmap 0)))

;drawn on through one, seen through the other, and the same as a canvas
;that has its pixels to itself
(defq a (Canvas-pixmap made) b (Canvas-pixmap found) plain (Canvas 64 32 1) empty (px-bytes made))
(each (lambda (canvas)
	(.-> canvas (:fill 0xff102030) (:set_color 0xffff8040) (:fbox 5 6 20 10)
		(:set_color 0xff00ff00) (:plot 60 30))) (list b plain))
(assert-true "drawing changed it" (nql (px-bytes made) empty))
(assert-eq "what one draws the other has" (px-bytes found) (px-bytes made))
(assert-eq "the same as a plain canvas" (px-bytes (getf plain +canvas_pixmap 0)) (px-bytes made))

;the one that made it goes. The other keeps the pixels, the name has gone
(defq before (px-bytes found))
(setq a :nil made :nil)
(assert-eq "the pixels outlast the one that made them" before (px-bytes found))
(assert-eq "the key went with the one that made it" :nil (pixmap-shared 64 32 key))

;a shader drawn straight into a pixmap is the same as one shaded into a
;string and put there a pixel at a time
(import "lib/gpu/vp.inc")
(defq program (shader-compile (shader-read (string-stream
		"(defun main :vec4 ((frag :vec2)) (vec4 (/ (:x frag) 64.0) (/ (:y frag) 32.0) 0.25 1.0))")))
	native (shader-vp program) frame (shader-vp-frame program native (list))
	direct (pixmap-shared 64 32 0) other (Canvas 64 32 1))
(each (lambda ((x y x1 y1))
	(shader-vp-draw native frame direct x y x1 y1 32)
	(. other :tile (shader-vp-argb native frame x y x1 y1 32) x y x1 y1))
	'((0 0 64 8) (0 8 64 20) (0 20 30 32) (30 20 64 32)))
;the pixels are the shader's own, to the bit
(assert-eq "a shader drawn straight into a pixmap" (shader-vp-argb native frame 0 0 64 8 32)
	(slice (px-bytes direct) 0 (* 64 8 4)))
;a pixel put there by :tile is premultiplied on the way, and comes out one
;level darker, so the two agree to within that
(assert-true "the same as a tile put there, to within a level"
	(every (# (<= (abs (- (code %0) (code %1))) 1)) (px-bytes direct) (px-bytes (getf other +canvas_pixmap 0))))
(assert-true "and it is a picture" (nql (px-bytes direct) (px-bytes (getf (Canvas 64 32 1) +canvas_pixmap 0))))
(assert-eq "a tile that is not inside the pixmap is not drawn" :nil (shader-vp-draw native frame direct 0 0 65 8 32))

;the clip of a canvas, set from Lisp, keeps a draw to a slice of the rows
(defq whole (Canvas 64 32 1) part (Canvas 64 32 1))
(assert-list-eq "a clip is cut down to the pixmap" '(0 8 64 32) (. (. part :set_clip -5 8 100 100) :get_clip))
(.-> part (:set_clip 0 8 64 16) (:set_color 0xff405060) (:fbox 0 0 64 32))
(.-> whole (:set_color 0xff405060) (:fbox 0 8 64 8))
(assert-eq "a draw is kept to the clip" (px-bytes (getf whole +canvas_pixmap 0)) (px-bytes (getf part +canvas_pixmap 0)))

