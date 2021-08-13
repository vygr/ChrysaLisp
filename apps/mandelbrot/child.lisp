;jit compile apps native functions
(import "sys/lisp.inc")
(jit "apps/mandelbrot/" "lisp.vp" '("depth"))

(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defun depth (x0 y0)
	(defq i -1 xc 0 yc 0 x2 0 y2 0)
	(while (and (/= (setq i (inc i)) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(setq yc (+ (mbfp-mul (mbfp-from-fixed 2.0) xc yc) y0) xc (+ (- x2 y2) x0)
			x2 (mbfp-mul xc xc) y2 (mbfp-mul yc yc))) i)

;native versions
(ffi depth "apps/mandelbrot/depth" 0)

(defun mandel (key mbox x y x1 y1 w h cx cy z)
	(write-int (defq reply (string-stream (cat ""))) (list x y x1 y1))
	(setq y (dec y))
	(while (/= (setq y (inc y)) y1)
		(defq xp (dec x))
		(while (/= (setq xp (inc xp)) x1)
			(write-char reply (depth (+ (mbfp-offset xp w z) cx) (+ (mbfp-offset y h z) cy))))
		(task-slice))
	(write-long reply key)
	(mail-send mbox (str reply)))

(defun main ()
	(defq select (alloc-select +select_size) running t +timeout 5000000)
	(while running
		(mail-timeout (elem +select_timeout select) +timeout 0)
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running nil))
			((= idx +select_main)
				;main mailbox, reset timeout and reply with result
				(mail-timeout (elem +select_timeout select) 0 0)
				(defq key (getf msg +job_key)
					mbox (getf msg +job_reply)
					msg (slice +job_x -1 msg))
				(apply mandel (cat (list key mbox) (map (lambda (_) (get-long msg (* _ +long_size))) (range 0 9)))))))
	(free-select select))
