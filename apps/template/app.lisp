;debug options
(case 2
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "././clipboard/app.inc")

(enums +event 0
	(enum close max min)
	(enum undo redo rewind cut copy paste)
	(enum main settings status info))

(enums +select 0
	(enum main tip timer))

(defq +rate (/ 1000000 1))

(ui-window *window* ()
	(ui-title-bar *title* "Template" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe9fe 0xe99d 0xe9ff 0xea08 0xe9c9 0xe9ca) +event_undo))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *tab_toolbar* (:font *env_window_font*)
			(ui-buttons ("main" "settings" "status" "info") +event_main))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow *tab_flow* (:flow_flags +flow_stack_fill :color +argb_black)
		(ui-backdrop *main_widget* (:min_width 512 :min_height 256
				:spacing 16 :style :grid
				:ink_color +argb_white))
		(ui-backdrop *settings_widget* (:ink_color +argb_red
				:spacing 16 :style :lines))
		(ui-backdrop *status_widget* (:ink_color +argb_green
				:spacing 16 :style :axis))
		(ui-backdrop *info_widget* (:ink_color +argb_blue
				:spacing 16 :style :plain))))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip select))
	(ui-tool-tips *main_toolbar*
		'("undo" "redo" "rewind" "cut" "copy" "paste"))
	(ui-tool-tips *tab_toolbar*
		'("main view" "settings view" "status view" "info view")))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (print _)(print) :t)))

(defun main ()
	(defq select (alloc-select +select_size) *running* :t mouse_state :u)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(tooltips)
	(mail-timeout (elem-get +select_timer select) +rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get +select_timer select) +rate 0))
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key)
					mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta))))
						;call bound control/command key action
						(when (defq action (. *key_map_control* :find key))
							(dispatch-action action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. *key_map_shift* :find key))
								(dispatch-action action))
							((<= +char_space key +char_tilde)
								;insert char etc ...
								(char key))))
					((defq action (. *key_map* :find key))
						;call bound key action
						(dispatch-action action))
					((<= +char_space key +char_tilde)
						;insert char etc ...
						(char key))))
			((and (= id (. *main_widget* :get_id))
				(= (getf *msg* +ev_msg_type) +ev_type_mouse))
					;mouse event in main widget
					(defq rx (getf *msg* +ev_msg_mouse_rx)
						ry (getf *msg* +ev_msg_mouse_ry))
					(cond
						((/= (getf *msg* +ev_msg_mouse_buttons) 0)
							;mouse button is down
							(case mouse_state
								(:d ;was down last time
									)
								(:u ;was up last time
									(setq mouse_state :d)))
							;use rx, ry ...
							)
						(:t ;mouse button is up
							(case mouse_state
								(:d ;was down last time
									(setq mouse_state :u))
								(:u ;was up last time, so we are hovering
									)))))
			(:t ;gui event
				(. *window* :event *msg*))))
	(profile-report "Template" :t)
	(gui-sub *window*)
	(free-select select))
