(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

;calculate the light and half-vectors entirely at compile-time!
(defq +l_vec (vector-norm (Vec3-r +real_-1 +real_-1 +real_-2)))
(defq +h_vec (vector-norm (vector-add +l_vec (Vec3-r +real_0 +real_0 +real_-1))))
(defq +anti_alias 3)

(defun generate-atom-image (key file)
	(defq size (* key +anti_alias) canvas (. (Canvas key key +anti_alias) :fill 0)
		r (* (n2r size) +real_1/2) iy -1)
	(while (< (++ iy) size)
		(task-slice)
		(defq y (+ (n2r iy) +real_1/2) ny (/ (- y r) r) ix -1)
		(while (< (++ ix) size)
			(defq x (+ (n2r ix) +real_1/2) nx (/ (- x r) r) d2 (+ (* nx nx) (* ny ny)))
			(when (<= d2 +real_1)
				(defq n_vec (Vec3-r nx ny (neg (sqrt (- +real_1 d2))))
					diffuse (max +real_0 (vector-dot n_vec +l_vec))
					spec_d (max +real_0 (vector-dot n_vec +h_vec))
					spec_d (* spec_d spec_d) spec_d (* spec_d spec_d)
					spec_d (* spec_d spec_d) spec_d (* spec_d spec_d)
					spec_d (* spec_d spec_d) spec_d (* spec_d spec_d)
					intensity (+ (const (n2r 0.3)) (* (const (n2r 0.7)) diffuse) spec_d)
					c (min 255 (n2i (* intensity (const (n2r 255.0))))))
				(.-> canvas
					(:set_color (+ 0xff000000 (<< c 16) (<< c 8) c))
					(:plot ix iy)))))
	;resize to final output (anti-aliased downsample from +anti_alias)
	(bind '(w h) (. canvas :pref_size))
	(canvas-save (. (Canvas w h 1) :resize canvas) file 32 :nil :t :t))

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