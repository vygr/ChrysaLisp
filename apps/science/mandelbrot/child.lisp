(defq *app_root* (path-to-file))
;the canvas alone, a child draws on the app's pixels and nothing else of
;the GUI
(import "lib/consts/colors.inc")
(import "gui/canvas/lisp.inc")
(import "lib/gpu/vp.inc")
(import "service/source/app.inc")
(import "./app.inc")

;the kernel the GPU runs, kernel.shader, as native code, the VP back end,
;in reals. It is read from the app's system, by its @Source if this task
;was sent by one, and assembled by the first child on this machine to
;want it
(defq program :nil native :nil)

(defun kernel ()
	(unless program
		(defq path (cat *app_root* "kernel.shader") text "" part ""
			stream (if (defq sid (get '*source_system*))
				(source-stream sid *source_service* path)
				(file-stream path)))
		(while (nempty? (setq part (ifn (read-blk stream 65536) ""))) (setq text (cat text part)))
		(setq program (shader-from-text text) native (shader-vp-kernel program)))
	native)

;the canvas of the app, on its pixels in shared memory, and the key they
;were found by
(defq shared_key 0 canvas :nil)

(defun attach (key w h)
	;the app's canvas, found again if the key has changed. :nil if this
	;node can not reach the pixels, it is on another machine say
	(unless (= key shared_key)
		(setq shared_key key canvas (and (/= key 0) (canvas-shared w h 1 key))))
	canvas)

(defun run (sq ox oy ow n dx dy)
	;n pixels of a square from ox oy, a step of dx dy each, worked out by
	;the kernel into the square's pixels. :t if all of them are inside
	(bind '(frame at_ox at_oy at_ow pal buf x y bw) sq)
	(defq out (str-alloc (* n +int_size)) solid :t)
	(set-long frame at_ox ox) (set-long frame at_oy oy) (set-long frame at_ow ow)
	(shader-vp-run program native frame 0 n (list pal out))
	(each (lambda (i)
			(defq c (get-uint out (* i +int_size)))
			(set-int buf (* (+ (- (+ ox (* i dx)) x) (* (- (+ oy (* i dy)) y) bw)) +int_size) c)
			(if (/= c +argb_black) (setq solid :nil)))
		(range 0 n))
	solid)

(defun mandel (key mbox x y x1 y1 w h canvas_key top pal_i cx cy z)
	;a square of the picture, scanned a ring at a time from its edge in. A
	;ring all inside the set is all there is within it, the set has no
	;holes, and the rest is black. A row of a ring is a run of the kernel,
	;as wide as the row, a column a run one wide, each index a row down
	(kernel)
	(defq found (attach canvas_key w h) bw (- x1 x) bh (- y1 y) pal (mandel-pal pal_i)
		buf (str-alloc (* bw bh +int_size)) names (map (const first) (first program))
		at_ox (* (find 'ox names) 8) at_oy (* (find 'oy names) 8) at_ow (* (find 'ow names) 8)
		frame (shader-vp-frame program native (list
			(list 'cx cx) (list 'cy cy) (list 'step (/ (* z +real_2) (n2r h)))
			(list 'w w) (list 'h h) (list 'top top) (list 'base +mandel_pal_base)
			(list 'rate +mandel_pal_rate) (list 'lbase +mandel_light_base)
			(list 'lgain +mandel_light_gain) (list 'ss +mandel_samples)))
		r 0 running :t inside 0 ix x iy y ix1 x1 iy1 y1)
	(defq sq (list frame at_ox at_oy at_ow pal buf x y bw))
	(while (and running (< (* r 2) bw) (< (* r 2) bh))
		(defq rx (+ x r) ry (+ y r) rx1 (- x1 r) ry1 (- y1 r) rw (- rx1 rx) rh (- ry1 ry)
			solid (run sq rx ry rw rw 1 0))
		;the bottom row, the left column and the right, each if there is one
		(if (> rh 1) (setq solid (and (run sq rx (dec ry1) rw rw 1 0) solid)))
		(when (> rh 2)
			(setq solid (and (run sq rx (inc ry) 1 (- rh 2) 0 1) solid))
			(if (> rw 1) (setq solid (and (run sq (dec rx1) (inc ry) 1 (- rh 2) 0 1) solid))))
		(if solid
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
	(job-child 5000000 (lambda (msg mbox)
		(apply mandel (getf-> msg +job_key +job_reply
			+rect_x +rect_y +rect_x1 +rect_y1 +rect_w +rect_h +rect_shared +rect_top
			+rect_pal +rect_cx +rect_cy +rect_z)))))
