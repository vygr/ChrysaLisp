(import "gui/pixmap/lisp.inc")
(import "lib/gpu/vp.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

;the shader as native code, the VP back end, assembled by the first
;child on this machine to ask for it
(defq program (shader-load +shader_file) native (shader-vp program))

;the pixels of the app's canvas, if they are in shared memory that this
;node can reach, and the key they were found by
(defq shared_key 0 shared :nil)

(defun attach (key width height)
	;the app's pixels, found again if the key has changed. :nil if this
	;node can not reach them, it is on another machine say
	(unless (= key shared_key)
		(setq shared_key key shared (and (/= key 0) (pixmap-shared width height key))))
	shared)

(defun rect (key mbox x y x1 y1 height width canvas_key inputs)
	;shade the tile, the canvas has y down, so the height is given. It is
	;shaded straight into the app's pixels if they can be reached, and only
	;the word that it is done goes back. If not the pixels go back.
	(defq frame (shader-vp-frame program native (shader-unpack program inputs))
		pixmap (attach canvas_key width height)
		data (if (and pixmap (shader-vp-draw native frame pixmap x y x1 y1 height)) ""
			(shader-vp-argb native frame x y x1 y1 height)))
	(mail-send mbox (cat data
		(char key +long_size) (char x +int_size) (char y +int_size)
		(char x1 +int_size) (char y1 +int_size))))

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
				(apply rect (push (getf-> msg +job_key +job_reply
						+job_x +job_y +job_x1 +job_y1 +job_height +job_width +job_shared)
					(slice msg +job_inputs -1)))))))
