(import "lib/debug/frames.inc")
;(import "lib/debug/profile.inc")

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "././clipboard/app.inc")

(enums +event 0
	(enum close max min)
	(enum undo redo rewind cut copy paste))

(enums +select 0
	(enum main tip timer))

(defq timer_rate (/ 1000000 1))

(ui-window *window* ()
	(ui-title-bar *title* "Template" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe9fe 0xe99d 0xe9ff 0xea08 0xe9ca 0xe9c9) +event_undo))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-backdrop main_widget (:color +argb_black :min_width 512 :min_height 256
			:ink_color +argb_white :spacing 16 :style :grid)))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("undo" "redo" "rewind" "cut" "copy" "paste")))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (print _)(print) t)))

(defun main ()
	(defq select (alloc-select +select_size) *running* t mouse_state :u)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(tooltips)
	(mail-timeout (elem-get +select_timer select) timer_rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get +select_timer select) timer_rate 0))
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
			((and (= id (. main_widget :get_id))
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
						(t  ;mouse button is up
							(case mouse_state
								(:d ;was down last time
									(setq mouse_state :u))
								(:u ;was up last time, so we are hovering
									)))))
			(t  ;gui event
				(. *window* :event *msg*))))
	(gui-sub *window*)
	(free-select select))
