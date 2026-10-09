(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./scene.inc")

(enums +event +event_user
	(enum mode count))

(enums +select 0
	(enum main task reply timer))

(defq +rate (/ 1000000 60) +slow_ticks 30 ticks 0 +retry_timeout (task-timeout 5)
	+min_opcodes 9 +max_opcodes 240 global_tick 0.0
	;the pixels of the canvas are in shared memory if the host has it, and
	;the children draw their slices straight onto them
	shared_canvas (canvas-shared +scene_width +scene_height 1)
	shared_key (if shared_canvas (canvas-key shared_canvas) 0)
	select :nil jobs :nil
	;farming, a frame is out with the children. warming, they have been
	;asked if they are up, and till they all are the frames are drawn
	;here, so the picture never stops for them
	farming :nil warming :nil
	frame_time 0 frame_drawn 0 frames 0 frames_us 0)

; Bouncer Particle: (model pos_v vel_v angle omega scale scale_phase squash_x squash_y)
; the model is the number of its opcode, see (opcode-model)
(defun create-bouncer (op_sym x y vx vy &optional scale)
	(defq index (if (num? op_sym) op_sym (opcode-index op_sym))
		ang (* (n2f (random 628)) 0.01)
		om (- (* (n2f (random 200)) 0.00015) 0.015)
		phase (* (n2f (random 628)) 0.01)
		s (ifn scale (+ 0.95 (* (n2f (random 25)) 0.01))))
	(bind '(name core_paths outline_paths glow_paths col_outline col_core col_glow hw hh)
		(opcode-model index))
	(defq ca (abs (cos ang)) sa (abs (sin ang))
		ext_x (* s (+ (* hw ca) (* hh sa)))
		ext_y (* s (+ (* hw sa) (* hh ca)))
		clamped_x (max ext_x (min x (- f_width ext_x)))
		clamped_y (max ext_y (min y (- f_height ext_y))))
	(list index
		(Vec2-f clamped_x clamped_y)
		(Vec2-f vx vy)
		ang om s phase 1.0 1.0))

(defun random-bouncer ()
	;one more, of any opcode, from anywhere, going any way
	(create-bouncer (random +opcode_count)
		(n2f (random +scene_width)) (n2f (random +scene_height))
		(* (- (n2f (random 400)) 200.0) 0.012) (* (- (n2f (random 400)) 200.0) 0.012)))

; Spawn an active flock of bouncers covering diverse opcode families proportionally
(defq bouncers (list
	(create-bouncer 'emit-call    (* f_width 0.25) (* f_height 0.25)  2.1  1.7 1.05)   ; Violet (Control)
	(create-bouncer 'emit-add-rr  (* f_width 0.72) (* f_height 0.28) -2.3  1.8 0.95)   ; Lime (Arithmetic)
	(create-bouncer 'emit-cpy-rr  (* f_width 0.50) (* f_height 0.50)  1.7 -2.0 1.10)   ; Amber (Data)
	(create-bouncer 'emit-sqrt-ff (* f_width 0.28) (* f_height 0.70) -1.9  2.2 1.00)   ; Cyan (Float)
	(create-bouncer 'emit-shl-rr  (* f_width 0.75) (* f_height 0.75) -2.0 -1.6 0.90)   ; Yellow (Bitwise)
	(create-bouncer 'emit-push    (* f_width 0.55) (* f_height 0.22)  2.3 -1.5 1.00)   ; Orange (Stack)
	(create-bouncer 'emit-slt-rr  (* f_width 0.35) (* f_height 0.45) -1.6 -2.1 0.95)   ; Hot Pink (Compare)
	(create-bouncer 'emit-alloc   (* f_width 0.70) (* f_height 0.55)  1.9  1.9 0.90)   ; Orange (Memory)
	(create-bouncer 'emit-div-rrr (* f_width 0.22) (* f_height 0.82)  2.2 -1.7 1.05))) ; Lime (Arithmetic)

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Opcodes" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *mode* ("One task" "All nodes") (:font *env_body_font*)) :connect +event_mode)
		(ui-label _ (:text "Opcodes" :font *env_body_font*))
		(. (ui-slider *count* (:maximum (- +max_opcodes +min_opcodes) :portion 30
			:value 15 :min_width 256)) :connect +event_count))
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-element *canvas* (ifn shared_canvas (Canvas +scene_width +scene_height 1)) (:color 0)))

(defun set-label (label text)
	;a label lays its text out once, so lay it out again for the new text
	(unless (eql (get :text label) text)
		(def label :text text)
		(.-> label :layout :dirty)))

(defun set-count ()
	;as many opcodes as the slider says, the first of them stay
	(defq want (+ +min_opcodes (get :value *count*)))
	(while (< (length bouncers) want) (push bouncers (random-bouncer)))
	(if (> (length bouncers) want) (setq bouncers (slice bouncers 0 want))))

(defun update-physics ()
	(++ global_tick 0.02)
	(each (lambda (b)
		(bind '(model pos vel angle omega scale phase sq_x sq_y) b)
		(bind '(name core_paths outline_paths glow_paths col_outline col_core col_glow hw hh) (opcode-model model))

		; Update position, rotation, and phase
		(vector-add pos vel pos)
		(setq angle (+ angle omega))
		(setq phase (+ phase 0.045))

		; Relax squash and stretch smoothly back to 1.0
		(setq sq_x (+ sq_x (* (- 1.0 sq_x) 0.12)))
		(setq sq_y (+ sq_y (* (- 1.0 sq_y) 0.12)))

		; Dynamic oriented bounding box extents in world space
		(defq ca (abs (cos angle)) sa (abs (sin angle))
			ext_x (* scale (+ (* hw ca) (* hh sa)))
			ext_y (* scale (+ (* hw sa) (* hh ca))))

		(bind '(x y) pos)
		(bind '(vx vy) vel)

		; Smooth wall reflection with occasional opcode morph on bounce
		; Left wall
		(when (and (< x ext_x) (< vx 0.0))
			(setq vx (abs vx)
				x ext_x
				sq_x 0.75 sq_y 1.25)
			(when (= 0 (random 2))
				(elem-set b 0 (random +opcode_count))))

		; Right wall
		(when (and (> x (- f_width ext_x)) (> vx 0.0))
			(setq vx (neg (abs vx))
				x (- f_width ext_x)
				sq_x 0.75 sq_y 1.25)
			(when (= 0 (random 2))
				(elem-set b 0 (random +opcode_count))))

		; Top wall
		(when (and (< y ext_y) (< vy 0.0))
			(setq vy (abs vy)
				y ext_y
				sq_x 1.25 sq_y 0.75)
			(when (= 0 (random 2))
				(elem-set b 0 (random +opcode_count))))

		; Bottom wall
		(when (and (> y (- f_height ext_y)) (> vy 0.0))
			(setq vy (neg (abs vy))
				y (- f_height ext_y)
				sq_x 1.25 sq_y 0.75)
			(when (= 0 (random 2))
				(elem-set b 0 (random +opcode_count))))

		; Write back updated values
		(elem-set pos +vec2_x x)
		(elem-set pos +vec2_y y)
		(elem-set vel +vec2_x vx)
		(elem-set vel +vec2_y vy)
		(elem-set b 3 angle)
		(elem-set b 6 phase)
		(elem-set b 7 sq_x)
		(elem-set b 8 sq_y)) bouncers))

(defun frame-done (how)
	;the frame is whole, show it and say how long it took
	(. *canvas* :swap +swap_write)
	(setq frames (inc frames) frames_us (+ frames_us (- (pii-time) frame_time)))
	(when (>= frames_us 500000)
		(set-label *status* (cat (str (/ frames_us frames 1000)) "."
			(str (% (/ frames_us frames 100) 10)) "ms a frame, "
			(str (length bouncers)) " opcodes, " how))
		(setq frames 0 frames_us 0)))

(defun slice-job (y y1 packed)
	;the message for some rows, with where every opcode is
	(cat (setf-> (str-alloc +slice_size)
		(+slice_shared shared_key) (+slice_y y) (+slice_y1 y1)) packed))

(defun warm-farm (&optional fresh)
	;every child is started again, one with no work for a while has gone,
	;unless they are fresh, only just started. Each is asked for a slice
	;of no rows, which it answers when it has the scene loaded and has
	;found the canvas
	(unless fresh (. jobs :restart))
	(setq warming :t farming :nil)
	(. jobs :add (map (lambda (&) (slice-job +scene_height +scene_height ""))
		(range 0 (. jobs :size)))))

(defun start-farm-frame ()
	;a frame drawn by the nodes, a slice for each of them, and where every
	;opcode is goes out with each slice
	(defq count (max 1 (length (lisp-nodes :t))) packed (scene-pack bouncers))
	(setq farming :t frame_time (pii-time) frame_drawn 0)
	(. jobs :add (map (# (slice-job (/ (* %0 +scene_height) count)
		(/ (* (inc %0) +scene_height) count) packed)) (range 0 count))))

(defun one-task-frame (how)
	;a frame drawn here, all of it
	(setq frame_time (pii-time))
	(scene-draw *canvas* (scene-pack bouncers) 0 +scene_height)
	(frame-done how))

(defun main ()
	(setq select (task-mboxes +select_size))
	(.-> *canvas*
		(:fill +scene_back)
		(:set_canvas_flags +canvas_flag_antialias)
		(:set_flags +view_flag_opaque +view_flag_opaque)
		(:swap +swap_write))
	(set-count)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	;the children are on the nodes of this machine, and no other. They draw
	;on the pixels of the canvas, in shared memory, and a node of another
	;machine, there is one when the machines are a mesh, can not reach them
	(setq jobs (Jobs (cat *app_root* "child.lisp")
		(elem-get select +select_task) (elem-get select +select_reply) (list 64)))
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
							(setq frame_drawn (+ frame_drawn (max 0 (getf msg +slice_reply_drawn))))
							(when (= out 0)
								(setq farming :nil)
								(frame-done (cat (str (length (lisp-nodes :t))) " nodes, "
									(str frame_drawn) " opcodes drawn over the slices")))))))
			(:t ;timer event, the next frame if the last is done
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(unless farming
					(set-count)
					(update-physics)
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
