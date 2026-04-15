;jit compile apps native functions
(defq *app_root* (path-to-file))
(jit *app_root* "lisp.vp" '("depth"))

(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defun depth (x0 y0)
	(defq i -1 xc +real_0 yc +real_0 x2 +real_0 y2 +real_0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) +real_4))
		(setq yc (+ (* +real_2 xc yc) y0) xc (+ (- x2 y2) x0)
			x2 (* xc xc) y2 (* yc yc))) i)

;native versions
(ffi (cat *app_root* "depth") depth)
; (depth x0 y0) -> cnt

; evaluates a pixel depth. if it deviates from the perimeter's 
; tracking color, it flags 'solid' as false to continue the ring scan inwards.
(defmacro eval-px-py (px py)
	`(progn
		(defq d (depth (+ (real-offset (n2r ,px) w z) cx)
					(+ (real-offset (n2r ,py) h z) cy)))
		(set-byte buf (+ (- ,px x) (* (- ,py y) bw)) d)
		(if (= ring_depth -1) (setq ring_depth d))
		(if (/= d ring_depth) (setq solid :nil))))

(defun mandel (key mbox x y x1 y1 w h cx cy z)
	(bind '(w h) (map (const n2r) (list w h)))
	(defq bw (- x1 x) bh (- y1 y) buf (str-alloc (+ (* bw bh) +job_reply_size))
		r 0 running :t fill_value -1 ix x iy y ix1 x1 iy1 y1)
	;scan perimeters
	(while (and running (< (* r 2) bw) (< (* r 2) bh))
		(defq rx (+ x r) ry (+ y r) rx1 (- x1 r) ry1 (- y1 r) solid :t ring_depth -1)
		;top edge
		(defq px (dec rx))
		(while (< (++ px) rx1) (eval-px-py px ry))
		;bottom edge (skips evaluating the same row if height is 1)
		(when (> ry1 (inc ry))
			(setq px (dec rx))
			(while (< (++ px) rx1) (eval-px-py px (dec ry1))))
		;left edge (skips corners)
		(defq py ry)
		(while (< (++ py) (dec ry1)) (eval-px-py rx py))
		;right edge (skips corners, checks width)
		(when (> rx1 (inc rx))
			(setq py ry)
			(while (< (++ py) (dec ry1)) (eval-px-py (dec rx1) py)))
		(if solid
			;uniform ring was found! 
			;we can safely short-circuit and flag the remaining inner bounds.
			(setq fill_value ring_depth ix rx iy ry ix1 rx1 iy1 ry1 running :nil)
			(++ r))
		(task-slice))
	;fill tail of the allocated msg and send
	(mail-send mbox (set-str buf (* bw bh)
		(setf-> (str-alloc +job_reply_size)
			(+job_reply_key key)
			(+job_reply_x x) (+job_reply_y y)
			(+job_reply_x1 x1) (+job_reply_y1 y1)
			(+job_reply_ix ix) (+job_reply_iy iy)
			(+job_reply_ix1 ix1) (+job_reply_iy1 iy1)
			(+job_reply_fill_value fill_value)))))

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
				(apply mandel (getf-> msg +job_key +job_reply
					+job_x +job_y +job_x1 +job_y1 +job_w +job_h
					+job_cx +job_cy +job_z))))))