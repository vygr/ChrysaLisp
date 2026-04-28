(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "lib/task/farm.inc")
(import "./app.inc")

(enums +event 0
	(enum close))

(enums +select 0
	(enum main task reply timer))

(defq +width 600 +height 600 +line_batch 4 +scale 1
	+timer_rate (/ 1000000 1) id :t dirty :nil
	+retry_timeout (task-timeout 5)
	+num_frames 40 frame_idx 0 z_start (n2r -3.0) z_dist (n2r 2.0)
	jobs (list) farm :nil lst_stream :nil select :nil)

(ui-window *window* ()
	(ui-title-bar _ "Raymarch" (0xea19) +event_close)
	(ui-canvas canvas +width +height +scale))

(defun dispatch-job (key val)
	;send another job to child
	(cond
		((defq job (pop jobs))
			(def val :job job :timestamp (pii-time))
			(mail-send (get :child val)
				(setf-> job
					(+job_key key)
					(+job_reply (elem-get select +select_reply)))))
		(:t ;no jobs in que
			(undef val :job :timestamp))))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task (const (cat *app_root* "child.lisp")) (elem-get nodes (random (length nodes)))
		+kn_call_child key (elem-get select +select_task)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(push jobs job)
		(undef val :job)))

(defun start-frame ()
	(defq fraction (/ (n2r frame_idx) (n2r +num_frames))
		cam_z (+ z_start (* z_dist fraction))
		light_z cam_z
		light_x (+ (n2r -0.1) (* (sin (* fraction +real_2pi)) (n2r 0.15))))
	(setq jobs (map (lambda (y1)
			(setf-> (str-alloc +job_size)
				(+job_x 0)
				(+job_y (- y1 (* +line_batch +scale)))
				(+job_x1 (* +width +scale))
				(+job_y1 y1)
				(+job_w (n2r (* +width +scale)))
				(+job_h (n2r (* +height +scale)))
				(+job_cam_z cam_z)
				(+job_light_x light_x)
				(+job_light_z light_z)))
		(range (* +height +scale) 0 (* +line_batch +scale))))
	(setq farm (Farm create destroy (* 2 (length (lisp-nodes)))))
	(mail-timeout (elem-get select +select_timer) +timer_rate 0))

(defun main ()
	(setq select (task-mboxes +select_size))
	(.-> canvas (:fill +argb_black) (:swap 0))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(setq lst_stream (file-stream "apps/media/films/data/raymarch.lst" +file_open_write))
	(start-frame)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					(:t (. *window* :event msg))))
			(+select_task
				;child launch response
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(def val :child child)
					(dispatch-job key val)))
			(+select_reply
				;child response
				(bind '(key x y x1 y1) (getf-> (slice msg (- -1 +job_reply) -1)
					+job_key +job_x +job_y +job_x1 +job_y1))
				(when (defq val (. farm :find key))
					(dispatch-job key val))
				(setq dirty :t)
				(. canvas :tile msg x y x1 y1))
			(:t ;timer event
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(if farm (. farm :refresh +retry_timeout))
				(when dirty
					(setq dirty :nil)
					(. canvas :swap +pixmap_mode_normal)
					(when (= 0 (length jobs))
						(defq working :nil)
						(if farm
							(. farm :each (lambda (key val)
								(setq working (or working (get :job val))))))
						(unless working
							(mail-timeout (elem-get select +select_timer) 0 0)
							(when lst_stream
								(defq cpm_name (cat "raymarch_" (str frame_idx) ".cpm")
									cpm_path (cat "apps/media/films/data/" cpm_name))
								(canvas-save canvas cpm_path 16 :t :t)
								; flip pixmap type back to Premultiplied (-32).
								; all our pixels are opaque (Alpha=0xFF), ARGB == Premul ARGB,
								; so we skip the expensive CPU math and GPU texture upload.
								(setf (getf canvas +canvas_pixmap 0) +pixmap_type -32 0)
								(write-line lst_stream cpm_path)
								(stream-flush lst_stream)
								(setq frame_idx (inc frame_idx))
								; Terminate old farm before next frame
								(if farm (. farm :close))
								(setq farm :nil)
								(if (< frame_idx +num_frames)
									(start-frame)
									(progn
										(write-line lst_stream "apps/media/films/data/raymarch_0.cpm")
										(stream-flush lst_stream)
										(setq lst_stream :nil))))))))))
	;close window and children
	(if farm (. farm :close))
	(gui-sub-rpc *window*))