(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/task/farm.inc")
(import "./scene.inc")

(enums +event 0
	(enum close mode shapes))

(enums +select 0
	(enum main task reply timer))

(structure +reply 0
	(long key)
	(uint y drawn))

(defq +rate (/ 1000000 60) +slow_ticks 30 ticks 0 +retry_timeout (task-timeout 5)
	+min_shapes 12 +max_shapes 1200
	;the pixels of the canvas are in shared memory if the host has it, and
	;the children draw their slices straight onto them
	shared_pixmap (pixmap-shared +scene_width +scene_height 0)
	shared_key (if shared_pixmap (pixmap-key shared_pixmap) 0)
	select :nil farm :nil jobs (list) slices (list) farming :nil
	start_time (pii-time) frame_time 0 frame_shapes 0 frames 0 frames_us 0)

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Canvas" (0xea19) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *mode* ("One task" "All nodes") (:font *env_body_font*)) :connect +event_mode)
		(ui-label _ (:text "Shapes" :font *env_body_font*))
		(. (ui-slider *shapes* (:maximum (- +max_shapes +min_shapes) :portion 100
			:value 180 :min_width 256)) :connect +event_shapes))
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-element *canvas* (if shared_pixmap (Canvas-pixmap shared_pixmap)
		(Canvas +scene_width +scene_height 1)) (:color 0)))

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
		+kn_call_run key (elem-get select +select_task)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (get :child val)) (mail-send child ""))
	(when (defq job (get :job val))
		(push jobs job)
		(undef val :job)))

(defun start-farm-frame ()
	;a frame drawn by the nodes, a slice for each of them. More slices than
	;that was slower when it was timed, a shape near the edge of a slice is
	;worked on by both sides of it
	(defq count (max 1 (length (lisp-nodes)))
		angle (n2i (* (scene-angle) 65536.0)) shapes (shape-count))
	(setq farming :t frame_time (pii-time) frame_shapes 0
		slices (map (# (/ (* %0 +scene_height) count)) (range 0 count))
		jobs (map (lambda (y)
			(setf-> (str-alloc +job_size)
				(+job_shared shared_key) (+job_angle angle) (+job_count shapes)
				(+job_y y) (+job_y1 (/ (* (inc (!)) +scene_height) count))))
			slices))
	(. farm :each (lambda (key val)
		(if (and (get :child val) (not (get :job val)))
			(dispatch-job key val)))))

(defun one-task-frame ()
	;a frame drawn here, all of it
	(setq frame_time (pii-time))
	(scene-draw *canvas* (scene-angle) (shape-count) 0 +scene_height)
	(frame-done "one task"))

(defun main ()
	(setq select (task-mboxes +select_size))
	(.-> *canvas* (:set_canvas_flags +canvas_flag_antialias) (:fill +argb_black) (:swap +swap_write))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(setq farm (Farm create destroy (max 1 (length (lisp-nodes)))))
	;it comes up on all the nodes, if their pixels can be shared
	(. *mode* :set_selected (if shared_pixmap 1 0))
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
						(when (and (= (. *mode* :get_selected) 1) (not shared_pixmap))
							(. *mode* :set_selected 0)
							(set-label *status* "This host has no shared memory for the nodes to draw on")))
					((. *window* :event msg))))
			(+select_task
				;child launch response
				(defq key (getf msg +kn_msg_key) child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(def val :child child)
					(dispatch-job key val)))
			(+select_reply
				;a slice is drawn
				(bind '(key y drawn) (getf-> msg +reply_key +reply_y +reply_drawn))
				(when (defq val (. farm :find key))
					(dispatch-job key val))
				(when (and farming (defq i (find y slices)))
					(setq slices (erase slices i (inc i)) frame_shapes (+ frame_shapes drawn))
					(when (empty? slices)
						(setq farming :nil)
						(frame-done (cat (str (length (lisp-nodes))) " nodes, "
							(str frame_shapes) " shapes drawn over the slices")))))
			(:t ;timer event, the next frame if the last is done
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(unless farming
					(if (and shared_pixmap (= (. *mode* :get_selected) 1))
						(start-farm-frame)
						(one-task-frame)))
				(when (= (setq ticks (% (inc ticks) +slow_ticks)) 0)
					(. farm :refresh +retry_timeout)))))
	(. farm :close)
	(gui-sub-rpc *window*))
