(import "././login/env.inc")
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/text/buffer.inc")
(import "lib/task/pipe.inc")

(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum copy paste paragraph))

(enums +select 0
	(enum main tip pipe))

(defq +vdu_min_width 60 +vdu_min_height 40
	+vdu_max_width 120 +vdu_max_height 40
	+state_filename "terminal_state.tre" *meta_map* (Fmap 31))

(ui-window *window* (:color 0xc0000000)
	(ui-title-bar *title* "Terminal" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe9ca 0xe9c9 0xe90d) +event_copy))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_left_fill)
		(. (ui-slider *yslider*) :connect +event_yscroll)
		(ui-flow *main_flow* (:flow_flags +flow_up_fill)
			(. (ui-slider *xslider*) :connect +event_xscroll))))

(defun input-poll ()
	(cond
		(*pipe*
			;active pipe running
			(. *pipe* :poll))
		(:t ;no active pipe running
			(mail-poll *select*))))

(defun input-select ()
	(cond
		(*pipe*
			;active pipe running
			(defq msg (. *pipe* :read))
			(cond
				((eql msg :t)
					;user select msg
					(defq msg (mail-read (elem-get (defq idx (mail-select *select*)) *select*))))
				(:t ;pipe closed or pipe data
					(defq idx +select_pipe))))
		(:t ;no active pipe running
			(defq msg (mail-read (elem-get (defq idx (mail-select *select*)) *select*)))))
	(list msg idx))

(defun refresh-display ()
	;load the vdu widgets with the text and selection
	(bind '(sx sy) (. *edit* :get_scroll))
	(.-> *edit* :get_buffer (:vdu_load (. *edit* :get_vdu_text) sx sy))
	(. *edit* :underlay_paper))

(defun refresh-sliders ()
	;set slider values
	(bind '(w h) (.-> *edit* :get_buffer :get_size))
	(bind '(sx sy) (. *edit* :get_scroll))
	(bind '(vw vh) (.-> *edit* :get_vdu_text :vdu_size))
	(defq smaxx (max 0 (- w vw -1)) smaxy (max 0 (- h vh))
		sx (max 0 (min sx smaxx)) sy (max 0 (min sy smaxy)))
	(def (. *xslider* :dirty) :maximum smaxx :portion vw :value sx)
	(def (. *yslider* :dirty) :maximum smaxy :portion vh :value sy)
	(. *edit* :set_scroll sx sy))

(defun refresh ()
	(unless (input-poll)
		;refresh display and ensure cursor is visible
		(bind '(cx cy) (. *edit* :get_cursor))
		(bind '(sx sy) (. *edit* :get_scroll))
		(bind '(w h) (.-> *edit* :get_vdu_text :vdu_size))
		(if (< cx sx) (setq sx cx))
		(if (< cy sy) (setq sy cy))
		(if (>= cx (+ sx w)) (setq sx (- cx w -1)))
		(if (>= cy (+ sy h)) (setq sy (- cy h -1)))
		(. *edit* :set_scroll sx sy)
		(refresh-sliders) (refresh-display)))

(defun window-resize ()
	;layout the window and size the vdu to fit
	(bind '(w h) (. *edit* :max_size))
	(set *edit* :vdu_width w :vdu_height h)
	(. *edit* :layout)
	(refresh-sliders) (refresh-display))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(set *edit* :vdu_width w :vdu_height h :min_width w :min_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(set *edit* :min_width +vdu_min_width :min_height +vdu_min_height)
	(. *window* :change_dirty x y w h)
	(window-resize))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip *select*))
	(ui-tool-tips *main_toolbar*
		'("copy" "paste" "select paragraph")))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun main ()
	(defq *select* (alloc-select +select_size)
		*cursor_x* 0 *cursor_y* 0 *running* :t *pipe* :nil
		*edit* (Terminal-edit)
		*meta_map* :nil *history_idx* (state-load))
	(. *edit* :set_select_color +argb_green6)
	(def *edit* :min_width +vdu_min_width :min_height +vdu_min_height
		:vdu_width +vdu_min_width :vdu_height +vdu_min_height :font *env_terminal_font*)
	(. *main_flow* :add_back *edit*)
	(tooltips)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(window-resize)
	(action-insert (cat "ChrysaLisp Terminal 2.0" (ascii-char +char_lf) *env_terminal_prompt*))
	(while *running*
		(bind '(*msg* idx) (input-select))
		(cond
			((= idx +select_pipe)
				;pipe event
				(cond
					((eql *msg* :nil)
						;pipe finished
						(. *pipe* :close)
						(setq *pipe* :nil)
						(action-insert (cat (ascii-char +char_lf) *env_terminal_prompt*)))
					((action-insert *msg*))))
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key) mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta))))
						;call bound control/command key action
						(when (defq action (. *key_map_control* :find key))
							(action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. *key_map_shift* :find key))
								(action))
							((<= +char_space key +char_tilde)
								(action-insert (char key)))))
					((defq action (. *key_map* :find key))
						;call bound key action
						(action))
					((<= +char_space key +char_tilde)
						;insert the char
						(action-insert (char key)))))
			(:t ;gui event
				(. *window* :event *msg*))))
	(if *pipe* (. *pipe* :close))
	(state-save)
	(free-select *select*)
	(gui-sub *window*))
