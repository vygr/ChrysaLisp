;jit compile apps native functions
(jit "apps/mandelbrot/" "lisp.vp" '("depth"))

(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defun depth (x0 y0)
	(defq i -1 xc +real_0 yc +real_0 x2 +real_0 y2 +real_0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) +real_4))
		(setq yc (+ (* +real_2 xc yc) y0) xc (+ (- x2 y2) x0)
			x2 (* xc xc) y2 (* yc yc))) i)

;native versions
(ffi "apps/mandelbrot/depth" depth)
; (depth x0 y0) -> cnt

(defun mandel (key mbox x y x1 y1 w h cx cy z)
	(write-int (defq reply (string-stream
		(str-alloc (+ (* (- x1 x) (- y1 y)) (* 4 +int_size))))) (list x y x1 y1))
	;convert to reals
	(bind '(w h) (map (const n2r) (list w h)))
	;cx, cy, and z (which were read as longs from the message)
	(bind '(cx cy z) (reals cx cy z))
	(defq y (dec y))
	(while (< (++ y) y1)
		(defq xp (dec x))
		(while (< (++ xp) x1)
			(write-char reply (depth (+ (real-offset (n2r xp) w z) cx) (+ (real-offset (n2r y) h z) cy))))
		(task-slice))
	(write-long reply key)
	(mail-send mbox (str reply)))

(defun main ()
	(defq select (task-mboxes +select_size) running :t +timeout 5000000)
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
				(apply mandel (cat (list key mbox) (map (lambda (_) (get-long msg (* _ +long_size))) (range 0 9))))))))