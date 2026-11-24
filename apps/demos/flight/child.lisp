(import "lib/math/mesh.inc")
(import "lib/math/scene.inc")
(import "./craft.inc")

;a child of Flight's farm: it makes a part of a craft, a mesh of its
;field, and sends it back as the shaders want it, a vertex where it is
;and its normal, smooth or of its face

(defun make-part (key mbox num flat command)
	(defq mesh (exec (first (read (string-stream command))))
		verts (if flat (mesh-corners mesh) (mesh-corners-smooth mesh))
		reply (setf-> (str-alloc (+ +part_reply_size (length verts)))
			(+job_reply_key key)
			(+part_reply_num num)))
	(mail-send mbox (cat (slice reply 0 +part_reply_size) verts)))

(defun main ()
	(job-child 1000000 (lambda (msg mbox)
			(make-part (getf msg +job_key) (getf msg +job_reply)
				(getf msg +part_num) (/= (getf msg +part_flat) 0) (slice msg +part_command -1)))))
