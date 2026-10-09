(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./scene.inc")

(enums +event 0
	(enum close mode shapes))

(enums +select 0
	(enum main task reply timer))

(defq +rate (/ 1000000 60) +slow_ticks 30 ticks 0 +retry_timeout (task-timeout 5)
	+min_shapes 12 +max_shapes 1200
	;the pixels of the canvas are in shared memory if the host has it, and
	;the children draw their slices straight onto them
	shared_canvas (canvas-shared +scene_width +scene_height 1)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0)
	select :nil jobs :nil
	;farming, a frame is out with the children. warming, they have been
	;asked if they are up, and till they all are the frames are drawn
	;here, so the picture never stops for them
	farming :nil warming :nil
	start_time (pii-time) frame_time 0 frame_shapes 0 frames 0 frames_us 0)

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Canvas" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *mode* ("One task" "All nodes") (:font *env_body_font*)) :connect +event_mode)
		(ui-label _ (:text "Shapes" :font *env_body_font*))
		(. (ui-slider *shapes* (:maximum (- +max_shapes +min_shapes) :portion 100
			:value 180 :min_width 256)) :connect +event_shapes))
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-element *canvas* (ifn shared_canvas (Canvas +scene_width +scene_height 1)) (:color 0)))

(defun set-label (label text)
	;a label lays its text out once, so lay it out again for the new text
	(unless (eql (get :text label) text)
		(def label :text text)
		(.-> label :layout :dirty)))

(defun shape-count ()
	(+ +min_shapes (get :value *shapes*)))

(defun scene-angle ()
	;the scene turns with the clock, not with how fast it is drawn
	(/ (n2f (/ (- (pii-time) start_time) 1000)) 4000.0))

(defun frame-done (how)
	;the frame is whole, show it and say how long it took
	(. *canvas* :swap +swap_write)
	(setq frames (inc frames) frames_us (+ frames_us (- (pii-time) frame_time)))
	(when (>= frames_us 500000)
		(set-label *status* (cat (str (/ frames_us frames 1000)) "."
			(str (% (/ frames_us frames 100) 10)) "ms a frame, "
			(str (shape-count)) " shapes, " how))
		(setq frames 0 frames_us 0)))

(defun slice-job (angle shapes y y1)
	(setf-> (str-alloc +slice_size)
		(+slice_shared shared_key) (+slice_angle angle) (+slice_count shapes)
		(+slice_y y) (+slice_y1 y1)))

(defun warm-farm (&optional fresh)
	;every child is started again, one with no work for a while has gone,
	;unless they are fresh, only just started. Each is asked for a slice
	;of no rows, which it answers when it has the scene loaded and has
	;found the canvas
	(unless fresh (. jobs :restart))
	(setq warming :t farming :nil)
	(. jobs :add (map (lambda (&) (slice-job 0 0 +scene_height +scene_height))
		(range 0 (. jobs :size)))))

(defun start-farm-frame ()
	;a frame drawn by the nodes, a slice for each of them. More slices than
	;that was slower when it was timed, a shape near the edge of a slice is
	;worked on by both sides of it
	(defq count (max 1 (length (lisp-nodes)))
		angle (n2i (* (scene-angle) 65536.0)) shapes (shape-count))
	(setq farming :t frame_time (pii-time) frame_shapes 0)
	(. jobs :add (map (# (slice-job angle shapes (/ (* %0 +scene_height) count)
		(/ (* (inc %0) +scene_height) count))) (range 0 count))))

(defun one-task-frame (how)
	;a frame drawn here, all of it
	(setq frame_time (pii-time))
	(scene-draw *canvas* (scene-angle) (shape-count) 0 +scene_height)
	(frame-done how))

(defun main ()
	(setq select (task-mboxes +select_size))
	(.-> *canvas* (:set_canvas_flags +canvas_flag_antialias) (:fill +argb_black) (:swap +swap_write))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(setq jobs (Jobs (cat *app_root* "child.lisp")
		(elem-get select +select_task) (elem-get select +select_reply)))
	;it comes up on all the nodes, if their pixels can be shared
	(. *mode* :set_selected (if shared_canvas 1 0))
	(if shared_canvas (warm-farm :t))
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(defq id :t)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_mode)
						(cond
							((/= (. *mode* :get_selected) 1))
							((not shared_canvas)
								(. *mode* :set_selected 0)
								(set-label *status* "This host has no shared memory for the nodes to draw on"))
							((not farming) (warm-farm))))
					((. *window* :event msg))))
			(+select_task
				;a child has started
				(. jobs :launched msg))
			(+select_reply
				;a slice is drawn, or a child has said it is up
				(when (defq out (. jobs :answered msg))
					(cond
						(warming (if (= out 0) (setq warming :nil)))
						(farming
							(setq frame_shapes (+ frame_shapes (getf msg +slice_reply_drawn)))
							(when (= out 0)
								(setq farming :nil)
								(frame-done (cat (str (length (lisp-nodes))) " nodes, "
									(str frame_shapes) " shapes drawn over the slices")))))))
			(:t ;timer event, the next frame if the last is done
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(unless farming
					(cond
						((or (not shared_canvas) (/= (. *mode* :get_selected) 1))
							(one-task-frame "one task"))
						(warming
							(one-task-frame "one task, while the nodes get ready"))
						((start-farm-frame))))
				(when (= (setq ticks (% (inc ticks) +slow_ticks)) 0)
					(. jobs :refresh +retry_timeout)))))
	(. jobs :close)
	(gui-sub-rpc *window*))
