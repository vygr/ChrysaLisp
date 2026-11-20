(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/gpu/render_farm.inc")
(import "./scene.inc")

(enums +event +event_user
	(enum mode shapes))

(enums +select 0
	(enum main timer))

(defq +rate (/ 1000000 60)
	+min_shapes 12 +max_shapes 1200
	;the pixels of the canvas are in shared memory if the host has it, and
	;the children draw their slices straight onto them
	shared_canvas (canvas-shared +scene_width +scene_height 1)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0)
	;what draws the frames, a (Render-work), lib/gpu/render_farm.inc: a
	;farm of child.lisp on the nodes, All nodes, or this task, One task
	select :nil render :nil
	start_time (pii-time) frame_time 0 frame_shapes 0 frames 0 frames_us 0
	;One task or All nodes, and how many shapes, as they were when last
	;closed, (config-save)
	*config* :nil +config_version 1 +config_file (cat (get :home *env*) "canvas.tre"))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Canvas" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *mode* ("One task" "All nodes") (:font :font_body)) :connect +event_mode)
		(ui-label _ (:text "Shapes" :font :font_body))
		(. (ui-slider *shapes* (:maximum (- +max_shapes +min_shapes) :portion 100
					:value 180 :min_width 256)) :connect +event_shapes))
	(ui-label *status* (:text "..." :font :font_body))
	(ui-element *canvas* (ifn shared_canvas (Canvas +scene_width +scene_height 1))))

(defun shape-count ()
	(+ +min_shapes (get :value *shapes*)))

(defun scene-angle ()
	;the scene turns with the clock, not with how fast it is drawn
	(/ (n2q (/ (- (pii-time) start_time) 1000)) 4000.0))

(defun frame-done (how)
	;the frame is whole and shown, say how long it took
	(setq frames (inc frames) frames_us (+ frames_us (- (pii-time) frame_time)))
	(when (>= frames_us 500000)
		(. *status* :set_text (cat (str (/ frames_us frames 1000)) "."
				(str (% (/ frames_us frames 100) 10)) "ms a frame, "
				(str (shape-count)) " shapes, " how))
		(setq frames 0 frames_us 0)))

(defun slice-job (angle shapes y y1)
	(setf-> (str-alloc +slice_size)
		(+slice_shared shared_key) (+slice_angle angle) (+slice_count shapes)
		(+slice_y y) (+slice_y1 y1)))

(defun frame-jobs (kind count ready)
	;the jobs of a frame for the nodes, a slice for each of them. More
	;slices than that was slower when it was timed, a shape near the edge
	;of a slice is worked on by both sides of it. Or, to ask if they are
	;ready, each a slice of no rows, which a child answers when it has the
	;scene loaded and has found the canvas
	(defq angle (scene-angle) shapes (shape-count))
	(setq frame_shapes 0)
	(if ready (map! (lambda () (slice-job 0.0 0 +scene_height +scene_height)) '() 0 count)
		(map! (# (slice-job angle shapes (/ (* (!) +scene_height) count)
				(/ (* (inc (!)) +scene_height) count))) '() 0 count)))

(defun next-frame ()
	;a frame of the moment, by the render: out with the nodes, or drawn here
	(setq frame_time (pii-time))
	(if (. render :frame)
		(frame-done (if (eql (. render :mode) :here) "one task" "one task, while the nodes get ready"))))

(defun config-load ()
	;as it was when last closed, or as it is the first time: all the nodes,
	;the slider where the window has it
	(setq *config* (with-read-lock +config_file
			(tree-load (file-stream +config_file))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (scatter (Emap) :version +config_version :nodes :t
			:shapes (get :value *shapes*))))
	(set *shapes* :value (max 0 (min (get :maximum *shapes*) (. *config* :find :shapes)))))

(defun config-save ()
	(scatter *config* :nodes (= (. *mode* :get_selected) 1) :shapes (get :value *shapes*))
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun main ()
	(setq select (task-mboxes +select_size) render (Render-work *canvas* (cat *app_root* "child.lisp")))
	(def render :jobs_fn (const frame-jobs)
		:here_fn (lambda (canvas) (scene-draw canvas (scene-angle) (shape-count) 0 +scene_height))
		:reply_fn (lambda (msg) (setq frame_shapes (+ frame_shapes (max 0 (getf msg +slice_reply_drawn))))))
	(config-load)
	(.-> *canvas* (:set_canvas_flags +canvas_flag_antialias) (:fill +argb_black) (:swap +swap_write))
	(. *window* :open)
	;it comes up on all the nodes if it was last time, and their pixels can
	;be shared
	(defq nodes (and shared_canvas (. *config* :find :nodes)))
	(. render :mode (if nodes :cpu :here))
	(. *mode* :set_selected (if nodes 1 0))
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(defq id :t out :nil)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				;a frame that was out with the nodes is whole
				(when (and out (not (. render :busy)))
					(setq out :nil)
					(frame-done (cat (str (length (lisp-nodes :t))) " nodes, "
							(str frame_shapes) " shapes drawn over the slices"))))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					((= id +event_mode)
						(cond
							((/= (. *mode* :get_selected) 1) (. render :mode :here) (setq out :nil))
							((not shared_canvas)
								(. *mode* :set_selected 0)
								(. *status* :set_text "This host has no shared memory for the nodes to draw on"))
							((. render :mode :cpu))))
					((. *window* :event msg))))
			(:t ;timer event, the next frame if the last is done
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(unless (. render :busy)
					(next-frame)
					(setq out (. render :busy)))
				(. render :tick))))
	(config-save)
	(. render :close)
	(. *window* :close))
