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
	*meta_map* (fmap 31))

(ui-window *window* (:color 0xc0000000)
	(ui-title-bar *title* "Terminal" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe9ca 0xe9c9 0xe90d) +event_copy))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_left_fill)
		(. (ui-slider *yslider*) :connect +event_yscroll)
		(ui-flow _ (:flow_flags +flow_up_fill)
			(. (ui-slider *xslider*) :connect +event_xscroll)
			(ui-flow stack_flow (:flow_flags +flow_stack_fill :font *env_terminal_font*)
				(ui-vdu *vdu_underlay* (:vdu_width +vdu_min_width :vdu_height +vdu_min_height
						:min_width 0 :min_height 0 :ink_color +argb_green))))))

(defun input-poll ()
	(cond
		(*pipe*
			;active pipe running
			(. *pipe* :poll))
		(t  ;no active pipe running
			(mail-poll *select*))))

(defun input-select ()
	(cond
		(*pipe*
			;active pipe running
			(defq msg (. *pipe* :read))
			(cond
				((eql msg t)
					;user select msg
					(defq msg (mail-read (elem (defq idx (mail-select *select*)) *select*))))
				(t  ;pipe closed or pipe data
					(defq idx +select_pipe))))
		(t  ;no active pipe running
			(defq msg (mail-read (elem (defq idx (mail-select *select*)) *select*)))))
	(list msg idx))

(defun clear-selection ()
	;clear the selection
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(cx cy) (. (. *edit* :get_buffer) :constrain cx cy))
	(. *edit* :set_anchor cx cy))

(defun load-display ()
	;load the vdu widgets with the text and selection
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(bind '(ax ay) (. *edit* :get_anchor))
	(. buffer :vdu_load *edit* sx sy)
	(. *vdu_underlay* :load
		(if (and (= cx ax) (= cy ay)) '() (. *edit* :create_underlay_selection))
		sx sy -1 -1))

(defun set-sliders ()
	;set slider values
	(bind '(w h) (. (. *edit* :get_buffer) :get_size))
	(bind '(sx sy) (. *edit* :get_scroll))
	(bind '(vw vh) (. *edit* :vdu_size))
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
		(bind '(w h) (. *edit* :vdu_size))
		(if (< cx sx) (setq sx cx))
		(if (< cy sy) (setq sy cy))
		(if (>= cx (+ sx w)) (setq sx (- cx w -1)))
		(if (>= cy (+ sy h)) (setq sy (- cy h -1)))
		(. *edit* :set_scroll sx sy)
		(set-sliders) (load-display)))

(defun window-resize ()
	;layout the window and size the vdu to fit
	(bind '(w h) (. *edit* :max_size))
	(set *edit* :vdu_width w :vdu_height h)
	(set *vdu_underlay* :vdu_width w :vdu_height h)
	(. *edit* :layout)
	(. *vdu_underlay* :layout)
	(set-sliders) (load-display))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(set *edit* :vdu_width w :vdu_height h :min_width w :min_height h)
	(set *vdu_underlay* :vdu_width w :vdu_height h :min_width w :min_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(set *edit* :min_width +vdu_min_width :min_height +vdu_min_height)
	(set *vdu_underlay* :min_width +vdu_min_width :min_height +vdu_min_height)
	(. *window* :change_dirty x y w h)
	(set-sliders) (load-display))

(defun tooltips ()
	(def *window* :tip_mbox (elem +select_tip *select*))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("copy" "paste" "select paragraph")))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun main ()
	(defq *select* (alloc-select +select_size)
		*cursor_x* 0 *cursor_y* 0 *running* t *pipe* nil
		*history* (list) *history_idx* 0 *edit* (Terminal-vdu))
	(. *edit* :set_buffer (Buffer t))
	(. *edit* :set_underlay_color +argb_green6)
	(. stack_flow :add_front *edit*)
	(tooltips)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(action-insert (cat "ChrysaLisp Terminal 2.0" (ascii-char +char_lf) *env_terminal_prompt*))
	(while *running*
		(bind '(*msg* idx) (input-select))
		(cond
			((= idx +select_pipe)
				;pipe event
				(cond
					((eql *msg* nil)
						;pipe finished
						(. *pipe* :close)
						(setq *pipe* nil)
						(action-insert (cat (ascii-char +char_lf) *env_terminal_prompt*)))
					((action-insert *msg*))))
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((defq id (getf *msg* +ev_msg_target_id) action (. event_map :find id))
				;call bound event action
				(action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key)
					(> (getf *msg* +ev_msg_key_keycode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key) mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_option +ev_key_mod_command))))
						;call bound control/command key action
						(when (defq action (. key_map_control :find key))
							(action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. key_map_shift :find key))
								(action))
							((<= +char_space key +char_tilda)
								(action-insert (char key)))))
					((defq action (. key_map :find key))
						;call bound key action
						(action))
					((<= +char_space key +char_tilda)
						;insert the char
						(action-insert (char key)))))
			(t  ;gui event
				(. *window* :event *msg*))))
	(if *pipe* (. *pipe* :close))
	(free-select *select*)
	(gui-sub *window*))
