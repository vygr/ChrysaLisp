;(import "lib/debug/frames.inc")
;(import "lib/debug/profile.inc")

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/math/scene.inc")

(enums +event 0
	(enum close max min)
	(enum mode auto)
	(enum xrot yrot zrot)
	(enum layout)
	(enum plain grid axis))

(enums +select 0
	(enum main tip timer))

(defq timer_rate (/ 1000000 30) +min_size 450 +max_size 800
	canvas_size +min_size canvas_scale 2 +radius +real_1
	*rotx* +real_0 *roty* +real_0 *rotz* +real_0
	+focal_dist (* +radius +real_2) +canvas_mode 0
	+near +focal_dist +far (+ +near (* +radius +real_2))
	+top +radius +bottom (* +radius +real_-1)
	+left (* +radius +real_-1) +right +radius
	*mol_index* 0 *auto_mode* nil *render_mode* nil *dirty* t scene nil
	palette (map (lambda (_) (fixeds
			(i2f (/ (logand (>> _ 16) 0xff) 0xff))
			(i2f (/ (logand (>> _ 8) 0xff) 0xff))
			(i2f (/ (logand _ 0xff) 0xff))))
		(list +argb_black +argb_white +argb_red +argb_green
			+argb_cyan +argb_blue +argb_yellow +argb_magenta)))

(ui-window *window* ()
	(ui-title-bar *title* "Cubes" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe962 0xea43) +event_mode))
		(ui-tool-bar style_toolbar ()
			(ui-buttons (0xe976 0xe9a3 0xe9f0) +event_plain))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-grid _ (:grid_width 1 :grid_height 3 :font *env_body_font*)
			(ui-label _ (:text "X rot:"))
			(ui-label _ (:text "Y rot:"))
			(ui-label _ (:text "Z rot:")))
		(ui-grid _ (:grid_width 1 :grid_height 3)
			(. (ui-slider xrot_slider (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_xrot)
			(. (ui-slider yrot_slider (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_yrot)
			(. (ui-slider zrot_slider (:value 0 :maximum 1000 :portion 10 :color +argb_green))
				:connect +event_zrot)))
	(ui-backdrop main_backdrop (:style :plain :color +argb_black :ink_color +argb_grey8
			:min_width +min_size :min_height +min_size)
		(ui-canvas main_widget canvas_size canvas_size canvas_scale)))

(defun tooltips ()
	(def *window* :tip_mbox (elem +select_tip select))
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
		(r2i (/ (* angle (const (i2r 1000))) +real_2pi))))

(defun get-rot (slider)
	(/ (* (i2r (get :value slider)) +real_2pi) (const (i2r 1000))))

(defun reset ()
	(setq scene (Scene "root") *dirty* t)
	(.-> scene
		(:add_node (defq sphere (Scene-object (gen-sphere +radius 10) (fixeds 1.0 0.0 0.0) "sphere1")))
		(:add_node (defq torus (Scene-object (gen-torus (- +radius +real_1/3) +real_1/3 15) (fixeds 0.0 1.0 0.0) "torus1"))))
	(. torus :set_translation +real_1/2 +real_1/2 (- +real_0 +focal_dist +radius +real_1/20))
	(. sphere :set_translation +real_-1/2 +real_-1/2 (- +real_0 +focal_dist +radius +real_1/20))
	(defq mesh (gen-sphere +real_1/4 8))
	(.-> torus
		(:add_node (defq sphere2 (Scene-object mesh (fixeds 1.0 0.0 1.0) "sphere2")))
		(:add_node (defq sphere3 (Scene-object mesh (fixeds 1.0 1.0 0.0) "sphere2"))))
	(. sphere2 :set_translation +real_0 +real_1/2 +real_0)
	(. sphere3 :set_translation +real_0 +real_-1/2 +real_0))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (print _)(print) t)))

(defun main ()
	(defq select (alloc-select +select_size) *running* t)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(.-> main_widget (:set_canvas_flags +canvas_mode) (:fill +argb_black) :swap)
	(radio-select style_toolbar 0)
	(gui-add-front (. *window* :change x y w h))
	(tooltips)
	(reset)
	(mail-timeout (elem +select_timer select) timer_rate 0)
	(while *running*
		(defq *msg* (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem +select_timer select) timer_rate 0)
				(when *auto_mode*
					(setq *rotx* (% (+ *rotx* (f2r 0.01)) +real_2pi)
						*roty* (% (+ *roty* (f2r 0.02)) +real_2pi)
						*rotz* (% (+ *rotz* (f2r 0.03)) +real_2pi)
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
	(gui-sub *window*)
	(free-select select)
	(if (get 'profile-report)
		(profile-report "Cubes")))
