(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

;calculate the light and half-vectors entirely at compile-time!
(defq +l_vec (vector-norm (Vec3-r +real_-1 +real_-1 +real_-2)))
(defq +h_vec (vector-norm (vector-add +l_vec (Vec3-r +real_0 +real_0 +real_-1))))

(defun generate-atom-image (key file)
	(defq size (* key 2) canvas (. (Canvas key key 2) :fill 0)
		r (* (n2r size) +real_1/2) iy -1)
	(while (< (++ iy) size)
		(task-slice)
		(defq y (+ (n2r iy) +real_1/2) ny (/ (- y r) r) ix -1)
		(while (< (++ ix) size)
			(defq x (+ (n2r ix) +real_1/2) nx (/ (- x r) r) d2 (+ (* nx nx) (* ny ny)))
			(if (<= d2 +real_1)
				(progn
					(defq nz (neg (sqrt (- +real_1 d2)))
						n_vec (Vec3-r nx ny nz)
						diffuse (max +real_0 (vector-dot n_vec +l_vec))
						spec_d (max +real_0 (vector-dot n_vec +h_vec))
						s2 (* spec_d spec_d) s4 (* s2 s2) s8 (* s4 s4)
						s16 (* s8 s8) s32 (* s16 s16) s64 (* s32 s32)
						intensity (+ (const (n2r 0.3)) (* (const (n2r 0.7)) diffuse) s64)
						c (min 255 (n2i (* intensity (const (n2r 255.0)))))
						col (+ 0xff000000 (<< c 16) (<< c 8) c))
					(.-> canvas (:set_color col) (:plot ix iy))))))
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