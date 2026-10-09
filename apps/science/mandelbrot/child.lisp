;jit compile apps native functions
(defq *app_root* (path-to-file))
(jit *app_root* "lisp.vp" '("shade"))

(import "gui/lisp.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

;the colour of a point, -1 if it does not get out of the set in top turns
(ffi (cat *app_root* "shade") shade)
; (shade x0 y0 top palette) -> -1 | argb

;the colour of a pixel, into the square. One that is not inside the set
;and the ring it is on is not all inside, the scan goes on inwards
(defmacro eval-px-py (px py)
	`(progn
		(defq d (shade (+ (real-offset (n2r ,px) w z) cx)
					(+ (real-offset (n2r ,py) h z) cy) top +mandel_pal)
			at (* (+ (- ,px x) (* (- ,py y) bw)) +int_size))
		(cond
			((= d -1) (set-int buf at +argb_black))
			(:t (set-int buf at d) (setq solid :nil)))))

;the canvas of the app, on its pixels in shared memory, and the key they
;were found by
(defq shared_key 0 canvas :nil)

(defun attach (key w h)
	;the app's canvas, found again if the key has changed. :nil if this
	;node can not reach the pixels, it is on another machine say
	(unless (= key shared_key)
		(setq shared_key key canvas (and (/= key 0) (canvas-shared w h 1 key))))
	canvas)

(defun mandel (key mbox x y x1 y1 w h canvas_key top cx cy z)
	(defq found (attach canvas_key w h))
	(bind '(w h) (map (const n2r) (list w h)))
	(defq bw (- x1 x) bh (- y1 y) buf (str-alloc (* bw bh +int_size))
		r 0 running :t inside 0 ix x iy y ix1 x1 iy1 y1)
	;scan perimeters
	(while (and running (< (* r 2) bw) (< (* r 2) bh))
		(defq rx (+ x r) ry (+ y r) rx1 (- x1 r) ry1 (- y1 r) solid :t)
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
			;a ring that is all inside the set, and so is all there is
			;within it, the set has no holes
			(setq inside 1 ix rx iy ry ix1 rx1 iy1 ry1 running :nil)
			(++ r))
		(task-slice))
	;the square is drawn straight onto the app's canvas if that can be
	;reached, and only the word that it is done goes back. If not, what
	;there is to draw goes back
	(defq whole (and (/= inside 0) (= ix x) (= iy y) (= ix1 x1) (= iy1 y1))
		reply (setf-> (str-alloc +rect_reply_size)
			(+job_reply_key key)
			(+rect_reply_x x) (+rect_reply_y y)
			(+rect_reply_x1 x1) (+rect_reply_y1 y1)
			(+rect_reply_ix ix) (+rect_reply_iy iy)
			(+rect_reply_ix1 ix1) (+rect_reply_iy1 iy1)
			(+rect_reply_inside inside)
			(+rect_reply_drawn (if found 1 0))))
	(cond
		(found
			(draw-rect found buf x y x1 y1 ix iy ix1 iy1 inside)
			(mail-send mbox reply))
		(whole (mail-send mbox reply))
		((mail-send mbox (cat reply buf)))))

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
					+rect_x +rect_y +rect_x1 +rect_y1 +rect_w +rect_h +rect_shared +rect_top
					+rect_cx +rect_cy +rect_z))))))
