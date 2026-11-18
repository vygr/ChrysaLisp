(defq *app_root* (path-to-file))
;the canvas alone, a child draws on the app's pixels and nothing else of
;the GUI
(import "lib/consts/colors.inc")
(import "gui/canvas/lisp.inc")
(import "lib/gpu/kernel.inc")
(import "./app.inc")

;The GPU's child, a farm of one where the CPU's is a farm of many, given
;the same job, a square of the picture, all of it as a rule. The kernel
;the CPU's children run as native code, kernel.shader, is run here on
;this node's GPU, a pixel a thread, in floats, the pixels read back and
;put on the app's canvas, its pixels in shared memory

;the kernel, the GPU's of it, the palette there and which it is, and a
;buffer of the GPU's for the pixels of a square, as big as the biggest
(defq program :nil kernel :nil pal_buf :nil pal_had -1 out_buf :nil out_size 0)

(defun kernel-up ()
	; (kernel-up) -> :nil | kernel
	;the kernel in the language of this GPU, and the palette's buffer, the
	;first time. :nil if this node has no GPU, or it can not take the kernel
	(unless program
		(setq program (shader-load (cat *app_root* "kernel.shader"))
			kernel (and (> (gpu-format) 0) (shader-gpu-kernel program))
			pal_buf (and kernel (gpu-buffer (* +mandel_pal_size +int_size)))))
	(and kernel pal_buf))

;the canvas of the app, on its pixels in shared memory, (canvas-attach)
(defq held (list))

(defun mandel (key mbox x y x1 y1 w h canvas_key top pal_i cx cy z)
	;a square of the picture, a run of the kernel over its pixels. It is
	;drawn straight onto the app's canvas if that can be reached, and only
	;the word that it is done goes back. If not, its pixels go back. A
	;square as wide as the picture, all of it as a rule, is read from the
	;GPU straight onto the canvas's pixels, no copy between
	(defq found (canvas-attach held w h 1 canvas_key) n (* (- x1 x) (- y1 y)) pixels :nil
		block (and found (= x 0) (= x1 w) (pixmap-buf (getf found +canvas_pixmap 0))))
	(unless (= pal_i pal_had)
		(gpu-write pal_buf 0 (mandel-pal (setq pal_had pal_i))))
	(when (> (* n +int_size) out_size)
		(if out_buf (gpu-buffer-free out_buf))
		(setq out_size (* n +int_size) out_buf (gpu-buffer out_size)))
	(and out_buf
		(shader-gpu-run program kernel
			(shader-pack program (list (list 'cx cx) (list 'cy cy)
				(list 'step (/ (* z +double_2) (n2d h)))
				(list 'w w) (list 'h h) (list 'ox x) (list 'oy y) (list 'ow (- x1 x))
				(list 'top top) (list 'base +mandel_pal_base) (list 'rate +mandel_pal_rate)
				(list 'lbase +mandel_light_base) (list 'lgain +mandel_light_gain)
				(list 'ss +mandel_samples)))
			0 n (list pal_buf out_buf))
		(setq pixels (if block (gpu-read out_buf 0 (* n +int_size) block (* y w +int_size))
			(gpu-read out_buf 0 (* n +int_size)))))
	(defq reply (setf-> (str-alloc +rect_reply_size)
			(+job_reply_key key)
			(+rect_reply_x x) (+rect_reply_y y)
			(+rect_reply_x1 x1) (+rect_reply_y1 y1)
			(+rect_reply_ix x) (+rect_reply_iy y)
			(+rect_reply_ix1 x1) (+rect_reply_iy1 y1)
			(+rect_reply_inside 0)
			(+rect_reply_drawn (if (and found pixels) 1 0))
			(+rect_reply_failed (if pixels 0 1))))
	(cond
		((not pixels) (mail-send mbox reply))
		(found
			(unless block (draw-rect found pixels x y x1 y1 x y x1 y1 0))
			(mail-send mbox reply))
		((mail-send mbox (cat reply pixels)))))

(defun main ()
	(job-child 5000000 (lambda (msg mbox)
			(if (kernel-up)
				(apply mandel (getf-> msg +job_key +job_reply
						+rect_x +rect_y +rect_x1 +rect_y1 +rect_w +rect_h +rect_shared +rect_top
						+rect_pal +rect_cx +rect_cy +rect_z))
				;a courtesy, so the farm need not wait: there is no GPU here
				(mail-send (getf msg +job_reply) (setf-> (str-alloc +rect_reply_size)
						(+job_reply_key (getf msg +job_key))
						(+rect_reply_failed 1))))))
	(if kernel (gpu-kernel-free kernel))
	(if pal_buf (gpu-buffer-free pal_buf))
	(if out_buf (gpu-buffer-free out_buf)))
