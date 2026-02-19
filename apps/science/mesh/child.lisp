(import "lib/math/mesh.inc")
(import "./app.inc")

(enums +select 0
	(enum main timeout))

(defun create-mesh (key mbox name command)
	(defq mesh (exec (first (read (string-stream command))))
		verts (. mesh :get_verts) norms (. mesh :get_norms) tris (. mesh :get_tris)
		reply_msg (setf-> (str-alloc (+ +job_reply_size
				(* (length verts) +long_size)
				(* (length norms) +long_size)
				(* (length tris) +long_size)))
			(+job_reply_key key)
			(+job_reply_num_verts (/ (length verts) 4))
			(+job_reply_num_norms (/ (length norms) 3))
			(+job_reply_num_tris (/ (length tris) 4))
			(+job_reply_name name))
		data (+ +str_data +job_reply_data))
	(each (# (obj-set reply_msg data +type_real %0) (++ data +long_size)) verts)
	(each (# (obj-set reply_msg data +type_real %0) (++ data +long_size)) norms)
	(each (# (obj-set reply_msg data +type_long %0) (++ data +long_size)) tris)
	(mail-send mbox reply_msg))

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
					(getf msg +job_name) (slice msg +job_command -1))))))
