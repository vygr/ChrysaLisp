(import "gui/lisp.inc")
(import "./scene.inc")

;the canvas of the app, on its pixels in shared memory, (canvas-attach)
(defq held (list))

(defun draw-slice (key mbox canvas_key angle count y y1)
	;draw the rows, and say how many shapes it took, -1 if the canvas
	;could not be reached. No rows at all is the app asking if this child
	;is up, with the scene loaded and the canvas found
	(defq canvas (canvas-attach held +scene_width +scene_height 1 canvas_key +canvas_flag_antialias))
	(mail-send mbox (setf-> (str-alloc +slice_reply_size)
		(+job_reply_key key)
		(+slice_reply_y y)
		(+slice_reply_drawn (cond
			((not canvas) -1)
			((>= y y1) 0)
			((scene-draw canvas angle count y y1)))))))

(defun main ()
	(job-child 5000000 (lambda (msg mbox)
		(apply draw-slice (getf-> msg +job_key +job_reply +slice_shared
			+slice_angle +slice_count +slice_y +slice_y1)))))
