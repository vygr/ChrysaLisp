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
			(defq pixmap (pixmap-shared +scene_width +scene_height key))
			(. (Canvas-pixmap pixmap) :set_canvas_flags +canvas_flag_antialias))))
	canvas)

(defun draw-slice (key mbox canvas_key angle count y y1)
	;draw the rows, and say how many shapes it took, -1 if the canvas
	;could not be reached
	(mail-send mbox (cat (char key +long_size) (char y +int_size)
		(char (if (attach canvas_key)
			(scene-draw canvas (/ (n2f angle) 65536.0) count y y1) -1) +int_size))))

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
				(apply draw-slice (getf-> msg +job_key +job_reply +job_shared
					+job_angle +job_count +job_y +job_y1))))))
