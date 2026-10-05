(import "lib/gpu/vp.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

;the shader as native code, the VP back end, assembled by the first
;child on this machine to ask for it
(defq program (shader-load +shader_file) native (shader-vp program))

(defun rect (key mbox x y x1 y1 height inputs)
	;shade the tile, the canvas has y down, so the height is given
	(mail-send mbox (cat
		(shader-vp-argb native (shader-vp-frame program native
			(shader-unpack program inputs)) x y x1 y1 height)
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
						+job_x +job_y +job_x1 +job_y1 +job_height)
					(slice msg +job_inputs -1)))))))
