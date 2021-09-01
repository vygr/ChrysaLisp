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
	*current_buffer* (Buffer t) *meta_map* (fmap 31) *underlay* (list)
	+selected (apply nums (map (lambda (_)
		(const (<< (canvas-from-argb32 +argb_green6 15) 48))) (str-alloc 8192)))
	+not_selected (nums-sub +selected +selected)
	+bracket_char (nums 0x7f))

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
			(ui-flow _ (:flow_flags +flow_stack_fill :font *env_terminal_font*)
				(ui-vdu *vdu* (:min_width +vdu_min_width :min_height +vdu_min_height
						:vdu_width +vdu_min_width :vdu_height +vdu_min_height
						:ink_color +argb_green))
				(ui-vdu *vdu_underlay* (:vdu_width +vdu_min_width :vdu_height +vdu_min_height
						:min_width 0 :min_height 0 :font (get :font *vdu*)
						:ink_color (get :ink_color *vdu*)))))))

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
	(bind '(x y) (. *current_buffer* :get_cursor))
	(bind '(x y) (. *current_buffer* :constrain x y))
	(setq *anchor_x* x *anchor_y* y))

(defun create-selection ()
	;create the underlay for block selection
	(bind '(x y) (. *current_buffer* :get_cursor))
	(defq x1 *anchor_x* y1 *anchor_y*)
	(if (> y y1) (defq st x x x1 x1 st st y y y1 y1 st))
	(and (= y y1) (> x x1) (defq st x x x1 x1 st))
	(cap (inc y1) (clear *underlay*))
	(defq uy -1 buffer (. *current_buffer* :get_text_lines))
	(while (< (setq uy (inc uy)) y) (push *underlay* ""))
	(cond
		((= y y1)
			(push *underlay* (cat (slice 0 x +not_selected) (slice x x1 +selected))))
		(t  (push *underlay* (cat
				(slice 0 x +not_selected)
				(slice x (inc (length (elem y buffer))) +selected)))
			(while (< (setq y (inc y)) y1)
				(push *underlay* (slice 0 (inc (length (elem y buffer))) +selected)))
			(push *underlay* (slice 0 x1 +selected)))))

(defun load-display ()
	;load the vdu widgets with the text and selection
	(. *current_buffer* :vdu_load *vdu* *scroll_x* *scroll_y*)
	(bind '(x y) (. *current_buffer* :get_cursor))
	(if (and (= x *anchor_x*) (= y *anchor_y*))
		(clear *underlay*) (create-selection))
	(. *vdu_underlay* :load *underlay* *scroll_x* *scroll_y* -1 -1))

(defun set-sliders ()
	;set slider values
	(bind '(w h) (. *current_buffer* :get_size))
	(bind '(vw vh) (. *vdu* :vdu_size))
	(defq smaxx (max 0 (- w vw -1)) smaxy (max 0 (- h vh)))
	(setq *scroll_x* (max 0 (min *scroll_x* smaxx)) *scroll_y* (max 0 (min *scroll_y* smaxy)))
	(def (. *xslider* :dirty) :maximum smaxx :portion vw :value *scroll_x*)
	(def (. *yslider* :dirty) :maximum smaxy :portion vh :value *scroll_y*))

(defun refresh ()
	(unless (input-poll)
		;refresh display and ensure cursor is visible
		(bind '(x y) (. *current_buffer* :get_cursor))
		(bind '(w h) (. *vdu* :vdu_size))
		(if (< x *scroll_x*) (setq *scroll_x* x))
		(if (< y *scroll_y*) (setq *scroll_y* y))
		(if (>= x (+ *scroll_x* w)) (setq *scroll_x* (- x w -1)))
		(if (>= y (+ *scroll_y* h)) (setq *scroll_y* (- y h -1)))
		(set-sliders) (load-display)))

(defun window-resize ()
	;layout the window and size the vdu to fit
	(bind '(w h) (. *vdu* :max_size))
	(set *vdu* :vdu_width w :vdu_height h)
	(set *vdu_underlay* :vdu_width w :vdu_height h)
	(. *vdu* :layout)
	(. *vdu_underlay* :layout)
	(set-sliders) (load-display))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(set *vdu* :vdu_width w :vdu_height h :min_width w :min_height h)
	(set *vdu_underlay* :vdu_width w :vdu_height h :min_width w :min_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(set *vdu* :min_width +vdu_min_width :min_height +vdu_min_height)
	(set *vdu_underlay* :min_width +vdu_min_width :min_height +vdu_min_height)
	(. *window* :change_dirty x y w h)
	(set-sliders) (load-display))

(defun tooltips ()
	(def *window* :tip_mbox (elem +select_tip *select*))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("copy" "paste" "select paragraph")))

;import actions and bindings
(import "./actions.inc")

(defun main ()
	(defq *select* (alloc-select +select_size)
		*cursor_x* 0 *cursor_y* 0 *anchor_x* 0 *anchor_y* 0 *scroll_x* 0 *scroll_y* 0
		*running* t mouse_state :u *pipe* nil *history* (list) *history_idx* 0)
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
			((and (= id (. *vdu* :get_id)) (= (getf *msg* +ev_msg_type) +ev_type_mouse))
				;mouse event on display
				(bind '(w h) (. *vdu* :char_size))
				(defq x (getf *msg* +ev_msg_mouse_rx) y (getf *msg* +ev_msg_mouse_ry))
				(setq x (if (>= x 0) x (- x w)) y (if (>= y 0) y (- y h)))
				(setq x (+ *scroll_x* (/ x w)) y (+ *scroll_y* (/ y h)))
				(cond
					((/= (getf *msg* +ev_msg_mouse_buttons) 0)
						;mouse button is down
						(case mouse_state
							(:d ;mouse drag event
								(bind '(x y) (. *current_buffer* :constrain x y))
								(. *current_buffer* :set_cursor x y)
								(refresh))
							(:u ;mouse down event
								(bind '(x y) (. *current_buffer* :constrain x y))
								(. *current_buffer* :set_cursor x y)
								(setq *anchor_x* x *anchor_y* y mouse_state :d)
								(refresh))))
					(t  ;mouse button is up
						(case mouse_state
							(:d ;mouse up event
								(defq click_count (getf *msg* +ev_msg_mouse_count))
								(cond
									((= click_count 2)
										(action-select-word))
									((= click_count 3)
										(action-select-line))
									((= click_count 4)
										(action-select-paragraph)))
								(setq mouse_state :u)
								(refresh))
							(:u ;mouse hover event
								)))))
			((and (= id (. *vdu* :get_id)) (= (getf *msg* +ev_msg_type) +ev_type_wheel))
				;wheel event on display area
				(setq *scroll_x* (+ *scroll_x* (getf *msg* +ev_msg_wheel_x))
					*scroll_y* (- *scroll_y* (getf *msg* +ev_msg_wheel_y)))
				(set-sliders) (load-display))
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
