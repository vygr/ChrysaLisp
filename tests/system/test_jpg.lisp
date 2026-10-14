(report-header "System: JPEG read, baseline, grey and Y Cb Cr 4:2:0 with restarts, against macOS's own reading of them")

(import "gui/canvas/lisp.inc")

(defun tj-pixels (canvas)
	(defq out (string-stream (cat "")))
	(pixmap-write (getf canvas +canvas_pixmap 0) out 32)
	(str out))

(defun tj-off (file mac)
	; (tj-off file mac) -> (most mean_by_100) | :nil
	;how far each byte of the pixels is from macOS's reading of the file
	(when (and (defq a (canvas-load file +load_flag_noswap)) (defq b (canvas-load mac +load_flag_noswap)))
		(defq pa (tj-pixels a) pb (tj-pixels b) most 0 sum 0)
		(when (= (length pa) (length pb))
			(each (lambda (i) (defq d (abs (- (code pa 1 i) (code pb 1 i)))) (setq most (max most d) sum (+ sum d)))
				(range 0 (length pa)))
			(list most (/ (* sum 100) (length pa))))))

(assert-list-eq "its size, and 32 bits a pixel" '(32 32 32) (canvas-info "tests/system/data/jpg_grey.jpg"))
(defq tj_grey (tj-off "tests/system/data/jpg_grey.jpg" "tests/system/data/jpg_grey_mac.png"))
(assert-true "grey: every pixel within 2 of macOS's" (and tj_grey (<= (first tj_grey) 2)))
(defq tj_420 (tj-off "tests/system/data/jpg_420.jpg" "tests/system/data/jpg_420_mac.png"))
;macOS makes the colour of a 4:2:0 smooth as it is made bigger, this takes the
;nearest, which differs at a sharp edge of colour
(assert-true "4:2:0 with restarts: within 8 on the whole" (and tj_420 (<= (second tj_420) 800)))
(assert-eq "not a .jpg is not taken for one" :nil (JPG-load (string-stream "not a jpeg at all")))
