(report-header "System: JPEG read, baseline and progressive, grey and Y Cb Cr 4:2:0 with restarts, against macOS's own reading of them")

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
			(each! (lambda () (defq d (abs (- (code pa 1 (!)) (code pb 1 (!))))) (setq most (max most d) sum (+ sum d))) '() 0 (length pa))
			(list most (/ (* sum 100) (length pa))))))

(assert-list-eq "its size, and 32 bits a pixel" '(32 32 32) (canvas-info "tests/system/data/jpg_grey.jpg"))
(defq tj_grey (tj-off "tests/system/data/jpg_grey.jpg" "tests/system/data/jpg_grey_mac.png"))
(assert-true "grey: every pixel within 2 of macOS's" (and tj_grey (<= (first tj_grey) 2)))
(defq tj_420 (tj-off "tests/system/data/jpg_420.jpg" "tests/system/data/jpg_420_mac.png"))
;macOS makes the colour of a 4:2:0 smooth as it is made bigger, this takes the
;nearest, which differs at a sharp edge of colour
(assert-true "4:2:0 with restarts: within 8 on the whole" (and tj_420 (<= (second tj_420) 800)))
(assert-eq "not a .jpg is not taken for one" :nil (JPG-load (string-stream "not a jpeg at all")))

;the same files made progressive by jpegtran, which keeps the numbers of
;every block as they were: the same pixels, to the byte
(defun tj-same (a b)
	(and (defq ca (canvas-load a +load_flag_noswap)) (defq cb (canvas-load b +load_flag_noswap))
		(eql (tj-pixels ca) (tj-pixels cb))))
;a file with no codes of its own has the standard's. These two were made with
;those, so with their DHT cut out they read the same
(defun tj-no-dht (file)
	; (tj-no-dht file) -> canvas | :nil
	(defq data (load file) at 2 out (slice data 0 2))
	(while (/= (code data 1 (inc at)) 0xda)
		(defq len (+ 2 (<< (code data 1 (+ at 2)) 8) (code data 1 (+ at 3))))
		(if (/= (code data 1 (inc at)) 0xc4) (setq out (cat out (slice data at (+ at len)))))
		(setq at (+ at len)))
	(JPG-load (string-stream (cat out (slice data at -1)))))
(each (lambda (file)
		(defq none (tj-no-dht file) with (canvas-load file +load_flag_noswap))
		(assert-true (cat file " with no DHT reads the same") (and none (eql (tj-pixels none) (tj-pixels with)))))
	'("tests/system/data/jpg_grey.jpg" "tests/system/data/jpg_420.jpg"))

(assert-true "progressive grey, the pixels of the baseline" (tj-same "tests/system/data/jpg_grey_prog.jpg" "tests/system/data/jpg_grey.jpg"))
(assert-true "progressive 4:2:0, the pixels of the baseline" (tj-same "tests/system/data/jpg_420_prog.jpg" "tests/system/data/jpg_420.jpg"))
