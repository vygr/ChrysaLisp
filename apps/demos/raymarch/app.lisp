(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "./app.inc")

(enums +event 0
	(enum close))

(enums +select 0
	(enum main task reply timer))

(defq +width 600 +height 600 +line_batch 4 +scale 1
	+timer_rate (/ 1000000 1) id :t dirty :nil
	+retry_timeout (task-timeout 5)
	+num_frames 40 frame_idx 0 z_start (n2r -3.0) z_dist (n2r 2.0)
	jobs :nil lst_stream :nil select :nil
	;the pixels of the canvas are in shared memory if the host has it, and
	;the children draw their tiles straight onto them
	shared_canvas (canvas-shared +width +height +scale)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0))

(ui-window *window* ()
	(ui-title-bar _ "Raymarch" (0xea19) +event_close)
	(ui-element canvas (ifn shared_canvas (Canvas +width +height +scale)) (:color 0)))

(defun start-frame ()
	(defq fraction (/ (n2r frame_idx) (n2r +num_frames))
		cam_z (+ z_start (* z_dist fraction))
		light_z cam_z
		light_x (+ (n2r -0.1) (* (sin (* fraction +real_2pi)) (n2r 0.15))))
	(. jobs :add (map (lambda (y1)
			(setf-> (str-alloc +tile_size)
				(+tile_x 0)
				(+tile_y (- y1 (* +line_batch +scale)))
				(+tile_x1 (* +width +scale))
				(+tile_y1 y1)
				(+tile_shared shared_key)
				(+tile_w (n2r (* +width +scale)))
				(+tile_h (n2r (* +height +scale)))
				(+tile_cam_z cam_z)
				(+tile_light_x light_x)
				(+tile_light_z light_z)))
		(range (* +height +scale) 0 (* +line_batch +scale))))
	(mail-timeout (elem-get select +select_timer) +timer_rate 0))

(defun frame-done ()
	;every tile of the frame is in, save it to the film and start the next
	(mail-timeout (elem-get select +select_timer) 0 0)
	(. canvas :swap +swap_write)
	(setq dirty :nil)
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
		(if (< frame_idx +num_frames)
			(start-frame)
			(progn
				(write-line lst_stream "apps/media/films/data/raymarch_0.cpm")
				(stream-flush lst_stream)
				(setq lst_stream :nil)))))

(defun main ()
	(setq select (task-mboxes +select_size))
	(.-> canvas (:fill +argb_black) (:swap +swap_write))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(setq lst_stream (file-stream "apps/media/films/data/raymarch.lst" +file_open_write)
		jobs (Jobs (cat *app_root* "child.lisp")
			(elem-get select +select_task) (elem-get select +select_reply)
			(* 2 (max 1 (length (lisp-nodes))))))
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
					((. *window* :event msg))))
			(+select_task
				;a child has started
				(. jobs :launched msg))
			(+select_reply
				;a tile is done
				(when (defq out (. jobs :answered msg))
					(setq dirty :t)
					;a child that could not reach the canvas sends the pixels
					(when (> (length msg) +tile_reply_size)
						(bind '(x y x1 y1) (getf-> msg +tile_reply_x +tile_reply_y
							+tile_reply_x1 +tile_reply_y1))
						(. canvas :tile (slice msg +tile_reply_pixels -1) x y x1 y1))
					(if (= out 0) (frame-done))))
			(:t ;timer event, show what there is of the frame so far
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(. jobs :refresh +retry_timeout)
				(when dirty
					(setq dirty :nil)
					(. canvas :swap +swap_write)))))
	;close window and children
	(. jobs :close)
	(gui-sub-rpc *window*))
