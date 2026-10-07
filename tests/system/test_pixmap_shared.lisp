(report-header "System: a pixmap with its pixels in shared memory")

(import "gui/lisp.inc")

(defun px-bytes (pixmap)
	;the pixels of a pixmap, as they are saved
	(defq stream (memory-stream))
	(pixmap-write pixmap stream 32)
	(stream-seek stream 0 0)
	(read-blk stream 1000000))

(defq key (pixmap-key) made (pixmap-shared 64 32 key 1) found (pixmap-shared 64 32 key 0))
(assert-true "made" made)
(assert-true "found by its name" found)
(assert-eq "one that was never made is not found" :nil (pixmap-shared 64 32 (cat key "x") 0))
(assert-eq "one that is smaller than asked for is not found" :nil (pixmap-shared 640 320 key 0))
(assert-eq "a name can only be made once" :nil (pixmap-shared 64 32 key 1))
(assert-true "two keys differ" (nql key (pixmap-key)))

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
(assert-eq "the name went with the one that made it" :nil (pixmap-shared 64 32 key 0))
