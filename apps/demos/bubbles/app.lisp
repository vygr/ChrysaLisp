(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/gpu/render_farm.inc")
(import "./scene.inc")

(enums +select 0
	(enum main timer tip))

(enums +event +event_user
	(enum reset)
	(enum style mode count))

(defq +min_width 300 +min_height 300
	+rate (/ 1000000 60)
	+min_bubbles 50 +max_bubbles 3000
	;the pixels of the canvas are in shared memory if the host has it, and
	;the children draw their slices straight onto them
	shared_canvas (canvas-shared +width +height 1)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0)
	;what draws the frames, a (Render-work), lib/gpu/render_farm.inc: a
	;farm of child.lisp on the nodes, All nodes, or this task, One task
	select :nil render :nil
	;the scene is its seed and how many bubbles, the app's copy of it is
	;made again when either changes
	seed (logand (pii-time) 0xffffff) scene_seed -1 scene_count -1 scene :nil
	start_time (pii-time) frame_time 0 frame_drawn 0 frames 0 frames_us 0
	;which way the light is, the mouse on the canvas moves it
	light_x -0.2357 light_y -0.2357)

(ui-window *window* ()
	(ui-title-bar _ "Bubbles" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_reset) +event_reset))
		(. (ui-radio-bar *style_toolbar* (+sym_plain +sym_grid +sym_axis)
				(:paper :paper_toolbar2)) :connect +event_style)
		(. (ui-radio-bar *mode* ("One task" "All nodes") (:font :font_body)) :connect +event_mode)
		(ui-label _ (:text "Bubbles" :font :font_body))
		(. (ui-slider *count* (:maximum (- +max_bubbles +min_bubbles) :portion 300
					:value 450 :min_width 128)) :connect +event_count))
	(ui-label *status* (:text "..." :font :font_body))
	(ui-scroll *image_scroll* +scroll_flag_both
		(:min_width +min_width :min_height +min_height :want_width +width :want_height +height)
		(ui-backdrop *backdrop* (:paper +argb_black :ink +argb_grey8)
			(ui-element *canvas* (ifn shared_canvas (Canvas +width +height 1))))))

(defun bubble-count ()
	(+ +min_bubbles (get :value *count*)))

(defun scene-time ()
	;the bubbles move with the clock, not with how fast they are drawn
	(/ (- (pii-time) start_time) 16667))

(defun frame-done (how)
	;the frame is whole and shown, say how long it took
	(setq frames (inc frames) frames_us (+ frames_us (- (pii-time) frame_time)))
	(when (>= frames_us 500000)
		(. *status* :set_text (cat (str (/ frames_us frames 1000)) "."
				(str (% (/ frames_us frames 100) 10)) "ms a frame, "
				(str (bubble-count)) " bubbles, " how))
		(setq frames 0 frames_us 0)))

(defun slice-job (count time y y1)
	(setf-> (str-alloc +slice_size)
		(+slice_shared shared_key) (+slice_seed seed) (+slice_count count)
		(+slice_time time)
		(+slice_light_x (n2i (* light_x 65536.0)))
		(+slice_light_y (n2i (* light_y 65536.0)))
		(+slice_y y) (+slice_y1 y1)))

(defun frame-jobs (kind count ready)
	;the jobs of a frame for the nodes, a slice for each of them. The
	;canvas is made clear here first, the bubbles are see through. Or, to
	;ask if they are ready, each a slice of no rows, which a child answers
	;when it has found the canvas and made the scene
	(setq frame_drawn 0)
	(cond
		(ready (map! (lambda () (slice-job (bubble-count) 0 +height +height)) '() 0 count))
		(:t (defq bubbles (bubble-count) time (scene-time))
			(. *canvas* :fill 0)
			(map! (# (slice-job bubbles time (/ (* (!) +height) count)
					(/ (* (inc (!)) +height) count))) '() 0 count))))

(defun frame-here (canvas)
	;a frame drawn here, all of it
	(defq count (bubble-count))
	(unless (and (= seed scene_seed) (= count scene_count))
		(setq scene_seed seed scene_count count scene (scene-make seed count)))
	(. canvas :fill 0)
	(scene-draw canvas scene (scene-time) light_x light_y 0 +height))

(defun next-frame ()
	;a frame of the moment, by the render: out with the nodes, or drawn here
	(setq frame_time (pii-time))
	(if (. render :frame)
		(frame-done (if (eql (. render :mode) :here) "one task" "one task, while the nodes get ready"))))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar*
		'("new bubbles"))
	(ui-tool-tips *style_toolbar*
		'("plain" "grid" "axis")))

(defun main ()
	;ui tree initial setup
	(setq select (task-mboxes +select_size) render (Render-work *canvas* (cat *app_root* "child.lisp")))
	(def render :jobs_fn (const frame-jobs) :here_fn (const frame-here)
		:reply_fn (lambda (msg) (setq frame_drawn (+ frame_drawn (max 0 (getf msg +slice_reply_drawn))))))
	(tooltips)
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)
	(. *backdrop* :set_size +width +height)
	(. *style_toolbar* :set_selected 0)
	(. *window* :open)
	;it comes up on all the nodes, if their pixels can be shared
	(. render :mode (if shared_canvas :cpu :here))
	(. *mode* :set_selected (if shared_canvas 1 0))

	;main event loop
	(defq id :t out :nil)
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with
		(bind '(idx *msg*) (. render :read select +select_timer))
		(cond
			((not idx)
				;a frame that was out with the nodes is whole
				(when (and out (not (. render :busy)))
					(setq out :nil)
					(frame-done (cat (str (length (lisp-nodes :t))) " nodes, "
							(str frame_drawn) " drawn over the slices"))))
			((= idx +select_timer)
				;timer event, the next frame if the last is done
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(unless (. render :busy)
					(next-frame)
					(setq out (. render :busy)))
				(. render :tick))
			((= idx +select_tip)
				;tip time mail
				(. *window* :tip *msg*))
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq id :nil))
			((= id +event_min)
				;min button
				(def *image_scroll* :want_width +min_width :want_height +min_height)
				(. *window* :fit))
			((= id +event_max)
				;max button
				(def *image_scroll* :want_width +width :want_height +height)
				(. *window* :fit))
			((= id +event_reset)
				;new bubbles, another seed
				(setq seed (logand (pii-time) 0xffffff)))
			((= id +event_mode)
				;one task, or all the nodes
				(cond
					;one task, the farm's work has stopped
					((/= (. *mode* :get_selected) 1) (. render :mode :here) (setq out :nil))
					((not shared_canvas)
						(. *mode* :set_selected 0)
						(. *status* :set_text "No shared memory on this host"))
					;all the nodes
					((. render :mode :cpu))))
			((= id +event_style)
				;styles
				(def (. *backdrop* :dirty) :style
						(elem-get '(:plain :grid :axis) (. *style_toolbar* :get_selected))))
			((and (= id (. *canvas* :get_id))
					(= (getf *msg* +ev_msg_type) +ev_type_mouse)
					(/= (getf *msg* +ev_msg_mouse_buttons) 0))
				;the mouse held down on the canvas moves the light
				(bind '(w h) (. *canvas* :get_size))
				(defq lx (n2q (* (- (getf *msg* +ev_msg_mouse_rx) (/ w 2)) 4))
					ly (n2q (* (- (getf *msg* +ev_msg_mouse_ry) (/ h 2)) 4))
					lz (const (* +box_size -4.0))
					len (sqrt (+ (* lx lx) (* ly ly) (* lz lz))))
				(setq light_x (/ lx len) light_y (/ ly len)))
			((. *window* :event *msg*))))
	;close window and children
	(. render :close)
	(. *window* :close))
