(import "gui/lisp.inc")
(import "lib/gpu/vp.inc")
(import "./atom.inc")

;A child of the Molecule app, ./app.lisp, of a farm on the nodes of this
;machine, away from the app's: it draws the image of an atom, a polished
;ball of its color, size and light, by the atom shader as native code, and
;sends its pixels. ./atom.inc has the job and the answer.

(defq atom_program (shader-load "apps/science/molecule/atom.shader")
	atom_native (shader-vp atom_program))

(defun draw (msg)
	(bind '(pixels kind level) (getf-> msg +atom_job_pixels +atom_job_kind +atom_job_level))
	(defq canvas (Canvas pixels pixels 1) pixmap (getf canvas +canvas_pixmap 0)
		out (string-stream (str-alloc (+ +atom_reply_size (* 4 pixels pixels)))))
	(shader-vp-draw atom_native
		(shader-vp-frame atom_program atom_native (list
				(list 'resolution (list pixels pixels))
				(list 'color (map (const n2d) (elem-get +palette kind)))
				;from two thirds of the light at the back to all of it at the front
				(list 'light_level (/ (n2d (+ level 10)) (const (n2d 15))))))
		pixmap 0 0 pixels pixels pixels :t)
	(write-blk out (setf-> (str-alloc +atom_reply_size) (+job_reply_key (getf msg +job_key))
		(+atom_reply_pixels pixels) (+atom_reply_kind kind) (+atom_reply_level level)))
	(pixmap-write pixmap out 32)
	(mail-send (getf msg +job_reply) (str out)))

(defun main ()
	;one with nothing to do for a while goes
	(job-child 5000000 (lambda (msg &ignore) (draw msg))))
