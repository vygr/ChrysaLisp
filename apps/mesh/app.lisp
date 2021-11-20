;(import "lib/debug/frames.inc")
;(import "lib/debug/profile.inc")

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/math/scene.inc")
(import "lib/task/farm.inc")
(import "./app.inc")

(enums +event 0
	(enum close max min)
	(enum mode auto)
	(enum xrot yrot zrot)
	(enum layout)
	(enum plain grid axis))

(enums +select 0
	(enum main task reply tip frame_timer retry_timer))

(defq anti_alias nil frame_timer_rate (/ 1000000 30) retry_timer_rate 1000000
	retry_timeout (if (starts-with "obj/vp64" (load-path)) 100000000 10000000)
	+min_size 450 +max_size 800
	canvas_size +min_size canvas_scale (if anti_alias 1 2)
	+canvas_mode (if anti_alias +canvas_flag_antialias 0)
	+stage_depth +real_4 +focal_dist +real_2
	*rotx* +real_0 *roty* +real_0 *rotz* +real_0
	+near +focal_dist +far (+ +near +stage_depth)
	+top (* +focal_dist +real_1/2) +bottom (* +focal_dist +real_-1/2)
	+left (* +focal_dist +real_-1/2) +right (* +focal_dist +real_1/2)
	*auto_mode* nil *render_mode* nil)

(ui-window *window* ()
	(ui-title-bar *title* "Mesh" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe962 0xea43) +event_mode))
		(ui-tool-bar style_toolbar ()
			(ui-buttons (0xe976 0xe9a3 0xe9f0) +event_plain))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-grid _ (:grid_width 1 :font *env_body_font*)
			(ui-label _ (:text "X rot:"))
			(ui-label _ (:text "Y rot:"))
			(ui-label _ (:text "Z rot:")))
		(ui-grid _ (:grid_width 1)
			(. (ui-slider xrot_slider (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_xrot)
			(. (ui-slider yrot_slider (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_yrot)
			(. (ui-slider zrot_slider (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_zrot)))
	(ui-backdrop main_backdrop (:style :plain :color +argb_black :ink_color +argb_grey8
			:min_width +min_size :min_height +min_size)
		(ui-canvas main_widget canvas_size canvas_size canvas_scale)))

(defun tooltips (mbox)
	(def *window* :tip_mbox mbox)
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("mode" "auto"))
	(each (# (def %0 :tip_text %1)) (. style_toolbar :children)
		'("plain" "grid" "axis")))

(defun radio-select (toolbar idx)
	(each (lambda (button)
			(undef (. button :dirty) :color)
			(if (= _ idx) (def button :color *env_radio_col*)))
		(. toolbar :children)) idx)

(defun set-rot (slider angle)
	(set (. slider :dirty) :value
		(n2i (/ (* angle (const (n2r 1000))) +real_2pi))))

(defun get-rot (slider)
	(/ (* (n2r (get :value slider)) +real_2pi) (const (n2r 1000))))

(defun dispatch-job (key val)
	;send another job to child
	(cond
		((defq job (pop jobs))
			(.-> val
				(:insert :job job)
				(:insert :timestamp (pii-time)))
			(mail-send (. val :find :child)
				(setf-> job
					(+job_key key)
					(+job_reply (elem-get +select_reply select)))))
		(t  ;no jobs in que
			(.-> val
				(:erase :job)
				(:erase :timestamp)))))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task "apps/mesh/child.lisp" (elem-get (random (length nodes)) nodes)
		+kn_call_child key (elem-get +select_task select)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (. val :find :child))
		(mail-send child ""))
	(when (defq job (. val :find :job))
		(push jobs job)
		(. val :erase :job)))

(defun create-scene (job_que)
	; (create-scene job_que) -> scene_root
	;create mesh loader jobs
	(each (lambda ((name command))
			(push job_que (cat (str-alloc +job_name) (pad name 16) command)))
		'(("sphere.1" "(Mesh-iso (Iso-sphere 20 20 20) (n2r 0.25))")
		("capsule" "(Mesh-iso (Iso-capsule 20 20 20) (n2r 0.25))")
		("cube.1" "(Mesh-iso (Iso-cube 8 8 8) (n2r 0.45))")
		("torus.1" "(Mesh-torus +real_1 +real_1/3 20)")
		("sphere.2" "(Mesh-sphere +real_1/2 10)")))
	;create scene graph
	(defq scene (Scene "root")
		sphere_obj (Scene-object nil (fixeds 1.0 1.0 1.0 1.0) "sphere.1")
		capsule1_obj (Scene-object nil (fixeds 0.8 1.0 0.0 0.0) "capsule.1")
		capsule2_obj (Scene-object nil (fixeds 0.8 0.0 1.0 1.0) "capsule.2")
		cube_obj (Scene-object nil (fixeds 0.8 1.0 1.0 0.0) "cube.1")
		torus_obj (Scene-object nil (fixeds 1.0 0.0 1.0 0.0) "torus.1")
		sphere2_obj (Scene-object nil (fixeds 0.8 1.0 0.0 1.0) "sphere.2"))
	(. sphere_obj :set_translation (+ +real_-1/3 +real_-1/3) (+ +real_-1/3 +real_-1/3) (- +real_0 +focal_dist +real_1))
	(. torus_obj :set_translation (+ +real_1/3 +real_1/3) (+ +real_1/3 +real_1/3) (- +real_0 +focal_dist +real_2))
	(. sphere2_obj :set_translation +real_0 +real_1/2 +real_0)
	(. cube_obj :set_translation +real_0 +real_-1/2 +real_0)
	(.-> capsule1_obj
		(:set_translation +real_0 +real_1/2 +real_0)
		(:set_rotation +real_0 +real_hpi +real_0))
	(. capsule2_obj :set_translation +real_0 +real_-1/2 +real_0)
	(.-> torus_obj (:add_node sphere2_obj) (:add_node cube_obj))
	(.-> sphere_obj (:add_node capsule1_obj) (:add_node capsule2_obj))
	(.-> scene (:add_node sphere_obj) (:add_node torus_obj)))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (print _)(print) t)))

(defun main ()
	;; (defq then (pii-time))
	;; (times 10 (Mesh-iso (Iso-capsule 30 30 30) (n2r 0.25)))
	;; (prin (time-in-seconds (- (pii-time) then)))(print)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(.-> main_widget (:set_canvas_flags +canvas_mode) (:fill +argb_black) :swap)
	(radio-select style_toolbar 0)
	(gui-add-front (. *window* :change x y w h))
	(defq select (alloc-select +select_size) *running* t *dirty* t
		jobs (list) scene (create-scene jobs) farm (Farm create destroy 4))
	(tooltips (elem-get +select_tip select))
	(mail-timeout (elem-get +select_frame_timer select) frame_timer_rate 0)
	(mail-timeout (elem-get +select_retry_timer select) retry_timer_rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_task)
				;child task launch responce
				(defq key (getf *msg* +kn_msg_key) child (getf *msg* +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(. val :insert :child child)
					(dispatch-job key val)))
			((= idx +select_reply)
				;child mesh responce
				(defq key (getf *msg* +job_reply_key)
					mesh_name (trim-start (slice +job_reply_name +job_reply_data *msg*))
					mesh (Mesh-data
							(getf *msg* +job_reply_num_verts)
							(getf *msg* +job_reply_num_norms)
							(getf *msg* +job_reply_num_tris)
							(slice +job_reply_data -1 *msg*)))
				(each (# (. %0 :set_mesh mesh)) (. scene :find_nodes mesh_name))
				(setq *dirty* t)
				(when (defq val (. farm :find key))
					(dispatch-job key val)))
			((= idx +select_retry_timer)
				;retry timer event
				(mail-timeout (elem-get +select_retry_timer select) retry_timer_rate 0)
				(. farm :refresh retry_timeout)
				(when (= 0 (length jobs))
					(defq working nil)
					(. farm :each (lambda (key val)
						(setq working (or working (. val :find :job)))))
					(unless working
						(mail-timeout (elem-get +select_retry_timer select) 0 0)
						(. farm :close))))
			((= idx +select_frame_timer)
				;frame timer event
				(mail-timeout (elem-get +select_frame_timer select) frame_timer_rate 0)
				(when *auto_mode*
					(setq *rotx* (% (+ *rotx* (n2r 0.01)) +real_2pi)
						*roty* (% (+ *roty* (n2r 0.02)) +real_2pi)
						*rotz* (% (+ *rotz* (n2r 0.03)) +real_2pi)
						*dirty* t)
					(set-rot xrot_slider *rotx*)
					(set-rot yrot_slider *roty*)
					(set-rot zrot_slider *rotz*))
				(when *dirty*
					(setq *dirty* nil)
					(. scene :set_rotation +real_0 +real_0 *rotz*)
					(each (# (. %0 :set_rotation *rotx* *roty* +real_0)) (. scene :children))
					(. scene :render main_widget (* canvas_size canvas_scale)
						+left +right +top +bottom +near +far *render_mode*)))
			;must be gui event to main mailbox
			((defq id (getf *msg* +ev_msg_target_id) action (. event_map :find id))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key)
					(> (getf *msg* +ev_msg_key_keycode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key)
					mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_option +ev_key_mod_command))))
						;call bound control/command key action
						(when (defq action (. key_map_control :find key))
							(dispatch-action action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. key_map_shift :find key))
								(dispatch-action action))
							((<= +char_space key +char_tilda)
								;insert char etc ...
								(char key))))
					((defq action (. key_map :find key))
						;call bound key action
						(dispatch-action action))
					((<= +char_space key +char_tilda)
						;insert char etc ...
						(char key))))
			(t  ;gui event
				(. *window* :event *msg*))))
	(. farm :close)
	(gui-sub *window*)
	(free-select select)
	(if (get 'profile-report)
		(profile-report "Mesh")))
