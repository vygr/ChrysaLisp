(import "gui/lisp.inc")
(import "./scene.inc")

;the canvas of the app, on its pixels in shared memory, (canvas-attach)
(defq held (list))

;the scene last asked for, and the seed and number of bubbles it is of
(defq scene_seed -1 scene_count -1 scene :nil)

(defun draw-slice (key mbox canvas_key seed count time light_x light_y y y1)
	;draw the rows, and say how many bubbles it took, -1 if the canvas
	;could not be reached. No rows at all is the app asking if this child
	;is up, with the canvas found and the scene made
	(unless (and (= seed scene_seed) (= count scene_count))
		(setq scene_seed seed scene_count count scene (scene-make seed count)))
	(defq canvas (canvas-attach held +width +height 1 canvas_key +canvas_flag_antialias))
	(mail-send mbox (setf-> (str-alloc +slice_reply_size)
			(+job_reply_key key)
			(+slice_reply_y y)
			(+slice_reply_drawn (cond
					((not canvas) -1)
					((>= y y1) 0)
					((scene-draw canvas scene time
							(/ (n2f light_x) 65536.0) (/ (n2f light_y) 65536.0) y y1)))))))

(defun main ()
	(job-child 5000000 (lambda (msg mbox)
			(apply draw-slice (getf-> msg +job_key +job_reply +slice_shared
					+slice_seed +slice_count +slice_time +slice_light_x +slice_light_y
					+slice_y +slice_y1)))))
