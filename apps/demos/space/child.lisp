(import "lib/math/scene.inc")
(import "./rock.inc")

;a child of Space's farm: it makes a rock of its seed, or the marker
;buoy, a mesh, and sends it back as the shaders want it, a vertex where it
;is and its normal, smooth, rock.inc

(defun make-rock (key mbox which command)
	;a rock of the seed the command is, or the buoy if it says so
	(defq verts (mesh-corners-smooth (cond ((eql command "buoy") (buoy-mesh)) ((eql command "missile") (missile-mesh)) ((rock-mesh (str-as-num command)))))
		reply (setf-> (str-alloc (+ +rock_reply_size (length verts)))
			(+job_reply_key key)
			(+rock_reply_num which)))
	(mail-send mbox (cat (slice reply 0 +rock_reply_size) verts)))

(defun main ()
	(job-child 1000000 (lambda (msg mbox)
			(make-rock (getf msg +job_key) (getf msg +job_reply) (getf msg +rock_num)
				(slice msg +rock_command -1)))))
