(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defun generate-atom-image (key file)
	(defq size (* key 2) canvas (. (Canvas key key 2) :fill 0)
		r (/ (n2r size) (n2r 2.0)) iy 0
		l_vec (vector-norm (Vec3-r (n2r -1.0) (n2r -1.0) (n2r -2.0)))
		v_vec (Vec3-r (n2r 0.0) (n2r 0.0) (n2r -1.0))
		h_vec (vector-norm (vector-add l_vec v_vec (Vec3-r (n2r 0.0) (n2r 0.0) (n2r 0.0)))))
	(while (< iy size)
		(task-slice)
		(defq y (+ (n2r iy) (n2r 0.5)) ny (/ (- y r) r) ix 0)
		(while (< ix size)
			(defq x (+ (n2r ix) (n2r 0.5)) nx (/ (- x r) r) d2 (+ (* nx nx) (* ny ny)))
			(if (<= d2 (n2r 1.0))
				(progn
					(defq nz (neg (sqrt (- (n2r 1.0) d2)))
						n_vec (Vec3-r nx ny nz)
						diffuse (max (n2r 0.0) (vector-dot n_vec l_vec))
						spec_d (max (n2r 0.0) (vector-dot n_vec h_vec))
						s2 (* spec_d spec_d) s4 (* s2 s2) s8 (* s4 s4)
						s16 (* s8 s8) s32 (* s16 s16) s64 (* s32 s32)
						intensity (+ (n2r 0.3) (* (n2r 0.7) diffuse) (* (n2r 1.0) s64))
						c (min 255 (n2i (* intensity (n2r 255.0))))
						col (+ 0xff000000 (<< c 16) (<< c 8) c))
					(.-> canvas (:set_color col) (:plot ix iy))))
			(++ ix))
		(++ iy))
	;resize to final output (anti-aliased downsample from 2x)
	(bind '(w h) (. canvas :pref_size))
	(canvas-save (. (Canvas w h 1) :resize canvas) file 32))

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
				(bind '(key atom_key reply) (getf-> msg +job_key +job_atom_key +job_reply))
				(generate-atom-image atom_key (slice msg +job_file -1))
				(mail-send reply (setf-> (str-alloc +job_reply_size)
					(+job_reply_key key)))))))