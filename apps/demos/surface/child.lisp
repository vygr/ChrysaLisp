(import "lib/gpu/cpu.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

;the shader as a lambda, the CPU back end
(defq program (shader-load +shader_file) shade (shader-cpu program)
	+real_255 (n2r 255) +zero4 (reals (n2r 0) (n2r 0) (n2r 0) (n2r 0))
	+one4 (reals (n2r 1) (n2r 1) (n2r 1) (n2r 1)))

(defun rect (key mbox x y x1 y1 height inputs)
	;shade the tile, the frag coord has y up, the canvas has y down
	(defq reply (string-stream (str-alloc (+ (* (+ (* (- x1 x) (- y1 y)) 4) +int_size) +long_size)))
		args (shader-cpu-args program (shader-unpack program inputs))
		tile (list x y x1 y1) y (dec y))
	(while (< (++ y) y1)
		(each (lambda (col)
				(bind '(r g b _) (map (const n2i)
					(nums-scale (nums-min (nums-max col +zero4) +one4) +real_255)))
				(write-int reply (+ 0xff000000 (<< r 16) (<< g 8) b)))
			(apply shade (cat (list x (- height y 1) x1 (- height y)) args))))
	(write-long reply key)
	(write-int reply tile)
	(mail-send mbox (str reply)))

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
