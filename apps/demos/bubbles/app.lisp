(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./scene.inc")

(enums +select 0
	(enum main task reply timer tip))

(enums +event 0
	(enum close max min)
	(enum reset)
	(enum style mode count))

(defq +min_width 300 +min_height 300
	+rate (/ 1000000 60) +slow_ticks 30 ticks 0 +retry_timeout (task-timeout 5)
	+min_bubbles 50 +max_bubbles 3000
	;the pixels of the canvas are in shared memory if the host has it, and
	;the children draw their slices straight onto them
	shared_canvas (canvas-shared +width +height 1)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0)
	select :nil jobs :nil
	;farming, a frame is out with the children. warming, they have been
	;asked if they are up, and till they all are the frames are drawn
	;here, so the picture never stops for them
	farming :nil warming :nil
	;the scene is its seed and how many bubbles, the app's copy of it is
	;made again when either changes
	seed (logand (pii-time) 0xffffff) scene_seed -1 scene_count -1 scene :nil
	start_time (pii-time) frame_time 0 frame_drawn 0 frames 0 frames_us 0
	;which way the light is, the mouse on the canvas moves it
	light_x -0.2357 light_y -0.2357)

(ui-window *window* ()
	(ui-title-bar _ "Bubbles" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe938) +event_reset))
		(. (ui-radio-bar *style_toolbar* (0xe976 0xe9a3 0xe9f0)
			(:color *env_toolbar2_col*)) :connect +event_style)
		(. (ui-radio-bar *mode* ("One task" "All nodes") (:font *env_body_font*)) :connect +event_mode)
		(ui-label _ (:text "Bubbles" :font *env_body_font*))
		(. (ui-slider *count* (:maximum (- +max_bubbles +min_bubbles) :portion 300
			:value 450 :min_width 128)) :connect +event_count))
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-scroll *image_scroll* +scroll_flag_both
			(:min_width +width :min_height +height)
		(ui-backdrop *backdrop* (:color +argb_black :ink_color +argb_grey8)
			(ui-element *canvas* (ifn shared_canvas (Canvas +width +height 1)) (:color 0)))))

(defun set-label (label text)
	;a label lays its text out once, so lay it out again for the new text
	(unless (eql (get :text label) text)
		(def label :text text)
		(.-> label :layout :dirty)))

(defun bubble-count ()
	(+ +min_bubbles (get :value *count*)))

(defun scene-time ()
	;the bubbles move with the clock, not with how fast they are drawn
	(/ (- (pii-time) start_time) 16667))

(defun frame-done (how)
	;the frame is whole, show it and say how long it took
	(. *canvas* :swap +swap_write)
	(setq frames (inc frames) frames_us (+ frames_us (- (pii-time) frame_time)))
	(when (>= frames_us 500000)
		(set-label *status* (cat (str (/ frames_us frames 1000)) "."
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

(defun warm-farm (&optional fresh)
	;every child is started again, one with no work for a while has gone,
	;unless they are fresh, only just started. Each is asked for a slice
	;of no rows, which it answers when it has found the canvas and made
	;the scene
	(unless fresh (. jobs :restart))
	(setq warming :t farming :nil)
	(. jobs :add (map (lambda (_) (slice-job (bubble-count) 0 +height +height))
		(range 0 (. jobs :size)))))

(defun start-farm-frame ()
	;a frame drawn by the nodes, a slice for each of them. The canvas is
	;made clear here first, the bubbles are see through
	(defq nodes (max 1 (length (lisp-nodes))) count (bubble-count) time (scene-time))
	(setq farming :t frame_time (pii-time) frame_drawn 0)
	(. *canvas* :fill 0)
	(. jobs :add (map (# (slice-job count time (/ (* %0 +height) nodes)
		(/ (* (inc %0) +height) nodes))) (range 0 nodes))))

(defun one-task-frame (how)
	;a frame drawn here, all of it
	(setq frame_time (pii-time))
	(defq count (bubble-count))
	(unless (and (= seed scene_seed) (= count scene_count))
		(setq scene_seed seed scene_count count scene (scene-make seed count)))
	(. *canvas* :fill 0)
	(scene-draw *canvas* scene (scene-time) light_x light_y 0 +height)
	(frame-done how))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar*
		'("new bubbles"))
	(ui-tool-tips *style_toolbar*
		'("plain" "grid" "axis")))

(defun main ()
	;ui tree initial setup
	(setq select (task-mboxes +select_size))
	(tooltips)
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)
	(. *backdrop* :set_size +width +height)
	(. *style_toolbar* :set_selected 0)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(def *image_scroll* :min_width +min_width :min_height +min_height)
	(setq jobs (Jobs (cat *app_root* "child.lisp")
		(elem-get select +select_task) (elem-get select +select_reply)))
	;it comes up on all the nodes, if their pixels can be shared
	(. *mode* :set_selected (if shared_canvas 1 0))
	(if shared_canvas (warm-farm :t))

	;main event loop
	(defq id :t)
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while id
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_timer)
				;timer event, the next frame if the last is done
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(unless farming
					(cond
						((or (not shared_canvas) (/= (. *mode* :get_selected) 1))
							(one-task-frame "one task"))
						(warming
							(one-task-frame "one task, while the nodes get ready"))
						((start-farm-frame))))
				(when (= (setq ticks (% (inc ticks) +slow_ticks)) 0)
					(. jobs :refresh +retry_timeout)))
			((= idx +select_task)
				;a child has started
				(. jobs :launched *msg*))
			((= idx +select_reply)
				;a slice is drawn, or a child has said it is up
				(when (defq out (. jobs :answered *msg*))
					(cond
						(warming (if (= out 0) (setq warming :nil)))
						(farming
							(setq frame_drawn (+ frame_drawn (getf *msg* +slice_reply_drawn)))
							(when (= out 0)
								(setq farming :nil)
								(frame-done (cat (str (length (lisp-nodes))) " nodes, "
									(str frame_drawn) " drawn over the slices")))))))
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq id :nil))
			((= id +event_min)
				;min button
				(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
				(. *window* :change_dirty x y w h))
			((= id +event_max)
				;max button
				(def *image_scroll* :min_width +width :min_height +height)
				(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
				(. *window* :change_dirty x y w h)
				(def *image_scroll* :min_width +min_width :min_height +min_height))
			((= id +event_reset)
				;new bubbles, another seed
				(setq seed (logand (pii-time) 0xffffff)))
			((= id +event_mode)
				;one task, or all the nodes
				(cond
					((/= (. *mode* :get_selected) 1))
					((not shared_canvas)
						(. *mode* :set_selected 0)
						(set-label *status* "No shared memory on this host"))
					((not farming) (warm-farm))))
			((= id +event_style)
				;styles
				(def (. *backdrop* :dirty) :style
					(elem-get '(:plain :grid :axis) (. *style_toolbar* :get_selected))))
			((and (= id (. *canvas* :get_id))
				(= (getf *msg* +ev_msg_type) +ev_type_mouse)
				(/= (getf *msg* +ev_msg_mouse_buttons) 0))
					;the mouse held down on the canvas moves the light
					(bind '(w h) (. *canvas* :get_size))
					(defq lx (n2f (* (- (getf *msg* +ev_msg_mouse_rx) (/ w 2)) 4))
						ly (n2f (* (- (getf *msg* +ev_msg_mouse_ry) (/ h 2)) 4))
						lz (* +box_size -4.0)
						len (sqrt (+ (* lx lx) (* ly ly) (* lz lz))))
					(setq light_x (/ lx len) light_y (/ ly len)))
			((. *window* :event *msg*))))
	;close window and children
	(. jobs :close)
	(gui-sub-rpc *window*))
