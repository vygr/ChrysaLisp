;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "apps/mandelbrot/mbmath.inc")

(defun-bind depth (x0 y0)
	(defq i -1 xc 0 yc 0 x2 0 y2 0)
	(while (and (/= (setq i (inc i)) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(setq yc (+ (mbfp-mul (mbfp-from-fixed 2.0) xc yc) y0) xc (+ (- x2 y2) x0)
			x2 (mbfp-mul xc xc) y2 (mbfp-mul yc yc))) i)

;native versions
(ffi depth "apps/mandelbrot/depth" 0)

(defun-bind mandel (x y x1 y1 w h cx cy z)
	(write-int (defq reply (string-stream (cat ""))) (list x y x1 y1))
	(setq y (dec y))
	(while (/= (setq y (inc y)) y1)
		(defq xp (dec x))
		(while (/= (setq xp (inc xp)) x1)
			(write-char reply (depth (+ (mbfp-offset xp w z) cx) (+ (mbfp-offset y h z) cy))))
		(task-sleep 0))
	(mail-send (str reply) mbox))

(defun-bind rect (mbox x y x1 y1 w h cx cy z tot)
	(cond
		((> (setq tot (/ tot 4)) 0)
			;split into more tasks
			(defq farm (open-farm "apps/mandelbrot/child.lisp" 3 kn_call_child)
				x2 (/ (+ x x1) 2) y2 (/ (+ y y1) 2))
			(mail-send (array mbox x y x2 y2 w h cx cy z tot) (elem 0 farm))
			(mail-send (array mbox x2 y x1 y2 w h cx cy z tot) (elem 1 farm))
			(mail-send (array mbox x y2 x2 y1 w h cx cy z tot) (elem 2 farm))
			(rect mbox x2 y2 x1 y1 w h cx cy z tot))
		(t	;do here
			(mandel x y x1 y1 w h cx cy z))))

(defun-bind main ()
	;read work request
	(defq msg (string-stream (mail-read (task-mailbox))))
	(apply rect (map (lambda (_) (read-long msg)) (range 0 11))))
