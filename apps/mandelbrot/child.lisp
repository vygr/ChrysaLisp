;jit compile apps native functions
(jit "apps/mandelbrot/" "lisp.vp" '("depth"))

(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defun depth (x0 y0)
	(defq i -1 xc 0 yc 0 x2 0 y2 0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(setq yc (+ (mbfp-mul (mbfp-from-fixed 2.0) xc yc) y0) xc (+ (- x2 y2) x0)
			x2 (mbfp-mul xc xc) y2 (mbfp-mul yc yc))) i)

;native versions
(ffi "apps/mandelbrot/depth" depth)

(defun mandel (key mbox x y x1 y1 w h cx cy z)
	(write-int (defq reply (string-stream (cat ""))) (list x y x1 y1))
	(setq y (dec y))
	(while (/= (++ y) y1)
		(defq xp (dec x))
		(while (/= (++ xp) x1)
			(write-char reply (depth (+ (mbfp-offset xp w z) cx) (+ (mbfp-offset y h z) cy))))
		(task-slice))
	(write-long reply key)
	(mail-send mbox (str reply)))

(defun main ()
	(defq select (alloc-select +select_size) running :t +timeout 5000000)
	(while running
		(mail-timeout (elem-get select +select_timeout) +timeout 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running :nil))
			((= idx +select_main)
				;main mailbox, reset timeout and reply with result
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(defq key (getf msg +job_key)
					mbox (getf msg +job_reply)
					msg (slice msg +job_x -1))
				(apply mandel (cat (list key mbox) (map (lambda (_) (get-long msg (* _ +long_size))) (range 0 9)))))))
	(free-select select))
