(import "lib/math/mesh.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defun create-mesh (key mbox name command)
	(defq mesh (exec (first (read (string-stream command))))
		verts (. mesh :get_verts) norms (. mesh :get_norms) tris (. mesh :get_tris)
		reply_list (cap (+ +job_reply_size
			(* (length verts) +vec4_size)
			(* (length norms) +vec3_size)
			(* (length tris) +tri_size)) (list)))
	(push reply_list
		(char key +long_size)
		(char (length verts) +int_size)
		(char (length norms) +int_size)
		(char (length tris) +int_size)
		name)
	(each (# (each (# (push reply_list (char %1 +long_size))) %0)) verts)
	(each (# (each (# (push reply_list (char %1 +long_size))) %0)) norms)
	(each (# (each (# (push reply_list (char %1 +int_size))) %0)) tris)
	(mail-send mbox (apply (const cat) reply_list)))

(defun main ()
	(defq select (task-mboxes +select_size) running :t +timeout 1000000)
	(while running
		(mail-timeout (elem-get select +select_timeout) +timeout 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running :nil))
			((= idx +select_main)
				;main mailbox, reset timeout and reply with mesh data
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(create-mesh (getf msg +job_key) (getf msg +job_reply)
					(slice msg +job_name +job_command) (slice msg +job_command -1))))))
