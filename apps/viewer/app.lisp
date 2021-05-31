(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/text/buffer.inc")

(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action)
	(enum file_folder_action file_leaf_action))

(defq +vdu_min_width 32 +vdu_min_height 16
	+vdu_max_width 100 +vdu_max_height 48
	*vdu_width* 80 *vdu_height* 48
	*current_buffer* (Buffer) *meta_map* (xmap 31)
	*current_file* nil *selected_file_node* nil)

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-scroll *file_tree_scroll* +scroll_flag_vertical nil
				(. (ui-tree *file_tree* +event_file_folder_action
						(:min_width 0 :color +argb_white :font *env_medium_terminal_font*))
					:connect +event_tree_action))
			(ui-backdrop _ (:color +argb_white)))
		(ui-flow _ (:flow_flags +flow_left_fill :font *env_terminal_font*)
			(. (ui-slider *yslider*) :connect +event_yscroll)
			(ui-flow _ (:flow_flags +flow_up_fill)
				(. (ui-slider *xslider*) :connect +event_xscroll)
				(ui-vdu *vdu* (:min_width *vdu_width* :min_height *vdu_height*
					:vdu_width *vdu_width* :vdu_height *vdu_height*
					:ink_color +argb_white))))))

(defun all-src-files (root)
	;all source files from root downwards, none recursive
	(defq stack (list root) files (list))
	(while (setq root (pop stack))
		(unless (starts-with "./obj" root)
			(each! 0 -1 (lambda (file type)
				(cond
					((eql type "8")
						;file
						(if (or ;src file ?
								(ends-with ".vp" file)
								(ends-with ".inc" file)
								(ends-with ".lisp" file)
								(ends-with ".md" file))
							(push files (cat (slice
								(if (eql root "./") 2 1) -1 root) "/" file))))
					(t  ;dir
						(unless (starts-with "." file)
							(push stack (cat root "/" file))))))
				(unzip (split (pii-dirlist root) ",") (list (list) (list))))))
	files)

(defun load-display ()
	;load the vdu widgets with the text
	(. *current_buffer* :vdu_load *vdu* *scroll_x* *scroll_y*))

(defun set-sliders ()
	;set slider values for current file
	(bind '(x y ax ay sx sy m ss) (. *meta_map* :find *current_file*))
	(bind '(w h) (. *current_buffer* :get_size))
	(defq smaxx (max 0 (- w *vdu_width* -1))
		smaxy (max 0 (- h *vdu_height* -1))
		sx (min sx smaxx) sy (min sy smaxy))
	(def (. *xslider* :dirty) :maximum smaxx :portion *vdu_width* :value sx)
	(def (. *yslider* :dirty) :maximum smaxy :portion *vdu_height* :value sy)
	(. *meta_map* :insert *current_file* (list x y ax ay sx sy m ss))
	(setq *scroll_x* sx *scroll_y* sy))

(defun refresh ()
	;refresh display and ensure cursor is visible
	(bind '(x y ax ay sx sy m ss) (. *meta_map* :find *current_file*))
	(bind '(x y) (. *current_buffer* :get_cursor))
	(bind '(w h) (. *vdu* :vdu_size))
	(if (< x sx) (setq sx x))
	(if (< y sy) (setq sy y))
	(if (>= x (+ sx w)) (setq sx (- x w -1)))
	(if (>= y (+ sy h)) (setq sy (- y h -1)))
	(. *meta_map* :insert *current_file* (list x y ax ay sx sy m ss))
	(set-sliders) (load-display))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	;must create a fresh buffer if not seen this before !
	(unless (. *meta_map* :find file)
		(defq m (if (ends-with ".md" file) t nil))
		(. *meta_map* :insert file (list 0 0 0 0 0 0 m nil)))
	(bind '(x y ax ay sx sy m ss) (. *meta_map* :find file))
	(when file
		(set *current_buffer* :mode m)
		(. *current_buffer* :file_load file))
	(setq *cursor_x* x *cursor_y* y *anchor_x* ax *anchor_y* ay mode m *shift_select* ss *current_file* file)
	(. *current_buffer* :set_cursor x y)
	(refresh)
	(def *title* :text (cat "Viewer -> " (if file file "<no file>")))
	(.-> *title* :layout :dirty))

(defun all-dirs (files)
	;return all the dir routes
	(reduce (lambda (dirs file)
		(defq dir (find-rev "/" file) dir (if dir (cat (slice 0 dir file) "/.")))
		(if (and dir (not (find dir dirs)))
			(push dirs dir) dirs)) files (list)))

(defun populate-file-tree ()
	;load up the file tree and a blank Buffer
	(defq all_src_files (sort cmp (all-src-files ".")))
	(each (# (. *file_tree* :add_route %0)) (all-dirs all_src_files))
	(each (# (. *file_tree* :add_route %0)) all_src_files)
	(populate-vdu nil)
	(select-node nil))

(defun window-resize (w h)
	;layout the window and size the vdu to fit
	(setq *vdu_width* w *vdu_height* h)
	(set *vdu* :vdu_width w :vdu_height h :min_width w :min_height h)
	(bind '(x y) (. *vdu* :get_pos))
	(bind '(w h) (. *vdu* :pref_size))
	(set *vdu* :min_width +vdu_min_width :min_height +vdu_min_height)
	(. *vdu* :change x y w h)
	(set-sliders) (load-display))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(setq *vdu_width* w *vdu_height* h)
	(set *vdu* :vdu_width w :vdu_height h :min_width w :min_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(set *vdu* :min_width +vdu_min_width :min_height +vdu_min_height)
	(. *window* :change_dirty x y w h)
	(set-sliders) (load-display))

(defun select-node (file)
	;highlight the selected file
	(if *selected_file_node* (undef (. *selected_file_node* :dirty) :color))
	(when file
		(setq *selected_file_node* (. *file_tree* :find_node file))
		(def (. *selected_file_node* :dirty) :color +argb_grey12))
	(bind '(w h) (. *file_tree* :pref_size))
	(. *file_tree* :change 0 0 w h)
	(def *file_tree* :min_width w)
	(def *file_tree_scroll* :min_width w)
	(.-> *file_tree_scroll* :layout :dirty_all))

;import actions and bindings
(import "apps/viewer/actions.inc")

(defun main ()
	(defq *cursor_x* 0 *cursor_y* 0 *anchor_x* 0 *anchor_y* 0 *scroll_x* 0 *scroll_y* 0
		mode nil *shift_select* nil *running* t mouse_state :u)
	(populate-file-tree)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(gui-add (. *window* :change x y w h))
	(refresh)
	(while *running*
		(cond
			((defq id (getf (defq *msg* (mail-read (task-mailbox))) +ev_msg_target_id)
					action (. event_map :find id))
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
							(:d ;was down last time
								(bind '(x y) (. *current_buffer* :constrain x y))
								(. *current_buffer* :set_cursor x y))
							(:u ;was up last time
								(bind '(x y) (. *current_buffer* :constrain x y))
								(. *current_buffer* :set_cursor x y)
								(setq *anchor_x* x *anchor_y* y *shift_select* t mouse_state :d))))
					(t  ;mouse button is up
						(case mouse_state
							(:d ;was down last time
								(setq mouse_state :u))
							(:u ;was up last time
								))))
				(refresh))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key)
					(> (getf *msg* +ev_msg_key_keycode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key) mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const (+ +ev_key_mod_control +ev_key_mod_command))))
						;call bound control/command key action
						(if (defq action (. key_map_control :find key)) (action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action
						(if (defq action (. key_map_shift :find key)) (action)))
					((defq action (. key_map :find key))
						;call bound key action
						(action))))
			(t  ;gui event
				(. *window* :event *msg*)))
		;update meta data
		(bind '(*cursor_x* *cursor_y*) (. *current_buffer* :get_cursor))
		(. *meta_map* :insert *current_file*
			(list *cursor_x* *cursor_y* *anchor_x* *anchor_y* *scroll_x* *scroll_y* mode *shift_select*)))
	(. *window* :hide))
