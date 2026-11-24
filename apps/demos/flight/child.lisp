(import "lib/math/mesh.inc")
(import "lib/math/scene.inc")
(import "./craft.inc")

;a child of Flight's farm: it makes a part of a craft, a mesh of its
;field, and sends it back as the shaders want it, a vertex where it is
;and its normal, smooth

(defun make-part (key mbox num command)
	(defq verts (mesh-corners-smooth (exec (first (read (string-stream command)))))
		reply (setf-> (str-alloc (+ +part_reply_size (length verts)))
			(+job_reply_key key)
			(+part_reply_num num)))
	(mail-send mbox (cat (slice reply 0 +part_reply_size) verts)))

(defun main ()
	(job-child 1000000 (lambda (msg mbox)
			(make-part (getf msg +job_key) (getf msg +job_reply)
				(getf msg +part_num) (slice msg +part_command -1)))))
