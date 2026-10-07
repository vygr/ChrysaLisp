(import "gui/lisp.inc")
(import "./scene.inc")

(enums +select 0
	(enum main timeout))

;the canvas of the app, on its pixels in shared memory, and the key they
;were found by
(defq shared_key 0 canvas :nil)

(defun attach (key)
	;the app's canvas, found again if the key has changed. :nil if this
	;node can not reach the pixels, it is on another machine say
	(unless (= key shared_key)
		(setq shared_key key canvas (and (/= key 0)
			(defq found (canvas-shared +scene_width +scene_height 1 key))
			(. found :set_canvas_flags +canvas_flag_antialias))))
	canvas)

(defun draw-slice (key mbox canvas_key angle count y y1)
	;draw the rows, and say how many shapes it took, -1 if the canvas
	;could not be reached. No rows at all is the app asking if this child
	;is up, with the scene loaded and the canvas found
	(mail-send mbox (setf-> (str-alloc +slice_reply_size)
		(+job_reply_key key)
		(+slice_reply_y y)
		(+slice_reply_drawn (cond
			((not (attach canvas_key)) -1)
			((>= y y1) 0)
			((scene-draw canvas (/ (n2f angle) 65536.0) count y y1)))))))

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
				;a slice to draw
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(apply draw-slice (getf-> msg +job_key +job_reply +slice_shared
					+slice_angle +slice_count +slice_y +slice_y1))))))
