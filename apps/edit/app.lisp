(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "apps/clipboard/app.inc")
(import "lib/consts/chars.inc")
(import "lib/text/buffer.inc")

(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum open_folder_action open_leaf_action)
	(enum undo redo rewind cut copy paste reflow tab_left tab_right)
	(enum prev next close_buffer save_all save new)
	(enum find_down find_up replace replace_all))

(defq *current_file* nil *selected_file_node* nil *selected_open_node* nil
	*vdu_width* 80 *vdu_height* 40 *meta_map* (xmap) *underlay* (list)
	*open_files* (list) *syntax* (Syntax)
	+vdu_min_width 40 +vdu_min_height 16 +vdu_max_width 100 +vdu_max_height 46
	+selected (apply nums (map (lambda (_)
		(const (<< (canvas-from-argb32 +argb_grey6 15) 48))) (str-alloc 8192)))
	+not_selected (nums-sub +selected +selected)
	+bracket_char (nums 0x7f) +state_filename "editor_open_files"
	+click_time 400000 then (pii-time) click_count 0)

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "Edit" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar _ ()
			(ui-buttons (0xe9fe 0xe99d 0xe9ff 0xea08 0xe9ca 0xe9c9 0xe909 0xe90a 0xe90b) +event_undo)
			(ui-buttons (0xe91d 0xe91e 0xe929 0xe97e 0xea07 0xe9f0) +event_prev
				(:color (const *env_toolbar2_col*))))
		(. (ui-textfield *name_text* (:hint_text "new file" :clear_text "" :color +argb_white))
			:connect +event_new))
	(ui-grid _ (:grid_width 2 :grid_height 1)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar _ ()
				(ui-buttons (0xe914 0xe91b) +event_find_down))
			(. (ui-textfield *find_text* (:hint_text "find" :clear_text "" :color +argb_white))
				:connect +event_find_down))
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar _ ()
				(ui-buttons (0xe95c 0xe95e) +event_replace))
			(. (ui-textfield *replace_text* (:hint_text "replace" :clear_text "" :color +argb_white))
				:connect +event_replace)))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-grid _ (:grid_width 1 :grid_height 2 :color +argb_grey14)
				(ui-flow _ (:flow_flags +flow_down_fill)
					(ui-label _ (:text "Open"))
					(ui-scroll *open_tree_scroll* +scroll_flag_vertical nil
						(. (ui-tree *open_tree* +event_open_folder_action
								(:min_width 0 :color +argb_white :font *env_medium_terminal_font*))
							:connect +event_tree_action)))
				(ui-flow _ (:flow_flags +flow_down_fill)
					(ui-label _ (:text "Project"))
					(ui-scroll *file_tree_scroll* +scroll_flag_vertical nil
						(. (ui-tree *file_tree* +event_file_folder_action
								(:min_width 0 :color +argb_white :font *env_medium_terminal_font*))
							:connect +event_tree_action))))
			(ui-backdrop _ (:color +argb_white)))
		(ui-flow _ (:flow_flags +flow_left_fill)
			(. (ui-slider *yslider*) :connect +event_yscroll)
			(ui-flow _ (:flow_flags +flow_up_fill)
				(. (ui-slider *xslider*) :connect +event_xscroll)
				(ui-flow _ (:flow_flags +flow_stack_fill :font *env_terminal_font*)
					(ui-vdu *vdu* (:min_width *vdu_width* :min_height *vdu_height*
						:vdu_width *vdu_width* :vdu_height *vdu_height*
						:ink_color +argb_white))
					(ui-vdu *vdu_underlay* (:vdu_width *vdu_width* :vdu_height *vdu_height*
						:min_width 0 :min_height 0
						:ink_color +argb_white)))))))

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

(defun clear-selection ()
	;clear the selection
	(bind '(x y) (. *current_buffer* :get_cursor))
	(bind '(x y) (. *current_buffer* :constrain x y))
	(setq *anchor_x* x *anchor_y* y *shift_select* nil))

(defun create-selection ()
	;create the underlay for block selection
	(bind '(x y) (. *current_buffer* :get_cursor))
	(defq x1 *anchor_x* y1 *anchor_y*)
	(if (> y y1)
		(defq x (logxor x x1) x1 (logxor x x1) x (logxor x x1)
			y (logxor y y1) y1 (logxor y y1) y (logxor y y1)))
	(and (= y y1) (> x x1)
		(defq x (logxor x x1) x1 (logxor x x1) x (logxor x x1)))
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

(defun create-brackets ()
	;create the underlay for just bracket indicators
	(clear *underlay*)
	(when (bind '(x y) (. *current_buffer* :left_bracket))
		(when (bind '(x1 y1) (. *current_buffer* :right_bracket))
			(cap (inc y1) *underlay*)
			(defq uy -1)
			(while (< (setq uy (inc uy)) y) (push *underlay* ""))
			(cond
				((= y y1)
					(push *underlay* (cat
						(slice 0 x +not_selected) +bracket_char
						(slice x (dec x1) +not_selected) +bracket_char)))
				(t  (push *underlay* (cat (slice 0 x +not_selected) +bracket_char))
					(while (< (setq y (inc y)) y1) (push *underlay* ""))
					(push *underlay* (cat (slice 0 x1 +not_selected) +bracket_char)))))))

(defun load-display ()
	;load the vdu widgets with the text and selection
	(. *current_buffer* :vdu_load *vdu* *scroll_x* *scroll_y*)
	(bind '(x y) (. *current_buffer* :get_cursor))
	(if (and (= x *anchor_x*) (= y *anchor_y*))
		(create-brackets)
		(create-selection))
	(. *vdu_underlay* :load *underlay* *scroll_x* *scroll_y* -1 -1))

(defun set-sliders ()
	;set slider values for current file
	(bind '(x y ax ay sx sy ss buffer) (. *meta_map* :find *current_file*))
	(bind '(w h) (. buffer :get_size))
	(defq smaxx (max 0 (- w *vdu_width* -1))
		smaxy (max 0 (- h *vdu_height* -1))
		sx (min sx smaxx) sy (min sy smaxy))
	(def (. *xslider* :dirty) :maximum smaxx :portion *vdu_width* :value sx)
	(def (. *yslider* :dirty) :maximum smaxy :portion *vdu_height* :value sy)
	(. *meta_map* :insert *current_file* (list x y ax ay sx sy ss buffer))
	(setq *scroll_x* sx *scroll_y* sy))

(defun refresh ()
	;refresh display and ensure cursor is visible
	(bind '(x y ax ay sx sy ss buffer) (. *meta_map* :find *current_file*))
	(bind '(x y) (. buffer :get_cursor))
	(bind '(w h) (. *vdu* :vdu_size))
	(if (< x sx) (setq sx x))
	(if (< y sy) (setq sy y))
	(if (>= x (+ sx w)) (setq sx (- x w -1)))
	(if (>= y (+ sy h)) (setq sy (- y h -1)))
	(. *meta_map* :insert *current_file* (list x y ax ay sx sy ss buffer))
	(set-sliders) (load-display))

(defun populate-file (file x y ax ay sx sy ss)
	;create new file buffer
	(unless (. *meta_map* :find file)
		(defq mode (if (ends-with ".md" file) t nil))
		(. *meta_map* :insert file
			(list x y ax ay sx sy ss (defq buffer (Buffer mode *syntax*))))
		(when file
			(. buffer :file_load file)
			(unless (find file *open_files*)
				(push *open_files* file)))))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(populate-file file 0 0 0 0 0 0 nil)
	(bind '(x y ax ay sx sy ss buffer) (. *meta_map* :find file))
	(setq *cursor_x* x *cursor_y* y *anchor_x* ax *anchor_y* ay *shift_select* ss
		*current_buffer* buffer *current_file* file)
	(. buffer :set_cursor x y)
	(refresh)
	(def *title* :text (cat "Edit -> " (if file file "<scratch pad>")))
	(.-> *title* :layout :dirty))

(defun all-dirs (files)
	;return all the dir routes
	(reduce (lambda (dirs file)
		(defq dir (find-rev "/" file) dir (if dir (cat (slice 0 dir file) "/.")))
		(if (and dir (not (find dir dirs)))
			(push dirs dir) dirs)) files (list)))

(defun populate-file-tree ()
	;load up the file tree
	(defq all_src_files (sort cmp (all-src-files ".")))
	(each (# (. *file_tree* :add_route %0)) (all-dirs all_src_files))
	(each (# (. *file_tree* :add_route %0)) all_src_files))

(defun populate-open-tree ()
	;reload open tree
	(sort cmp *open_files*)
	(each (# (. %0 :sub)) (. *open_tree* :children))
	(each (# (. *open_tree* :add_route %0)) (all-dirs *open_files*))
	(each (# (populate-vdu %0) (. *open_tree* :add_route %0)) *open_files*))

(defun load-open-files ()
	;load users open file tree
	(when (defq stream (file-stream (cat *env_home* +state_filename)))
		(each-line (lambda (line)
				(bind '(form _) (read (string-stream line) +char_space))
				(bind '(file (x y ax ay sx sy ss)) form)
				(populate-file file x y ax ay sx sy ss))
			stream)))

(defun save-open-files ()
	;save users open file tree
	(when (defq stream (file-stream (cat *env_home* +state_filename) +file_open_write))
		(each (lambda (file)
				(write-line stream (str (list file (slice 0 -2 (. *meta_map* :find file))))))
			(sort cmp *open_files*))))

(defun window-resize (w h)
	;layout the window and size the vdu to fit
	(setq *vdu_width* w *vdu_height* h)
	(set *vdu* :vdu_width w :vdu_height h :min_width w :min_height h)
	(set *vdu_underlay* :vdu_width w :vdu_height h)
	(bind '(x y) (. *vdu* :get_pos))
	(bind '(w h) (. *vdu* :pref_size))
	(set *vdu* :min_width +vdu_min_width :min_height +vdu_min_height)
	(set *vdu_underlay* :min_width +vdu_min_width :min_height +vdu_min_height)
	(. *vdu* :change x y w h)
	(. *vdu_underlay* :change x y w h)
	(set-sliders) (load-display))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(setq *vdu_width* w *vdu_height* h)
	(set *vdu* :vdu_width w :vdu_height h :min_width w :min_height h)
	(set *vdu_underlay* :vdu_width w :vdu_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(set *vdu* :min_width +vdu_min_width :min_height +vdu_min_height)
	(set *vdu_underlay* :min_width +vdu_min_width :min_height +vdu_min_height)
	(. *window* :change_dirty x y w h)
	(set-sliders) (load-display))

(defun select-node (file)
	;highlight the selected file
	(if *selected_file_node* (undef (. *selected_file_node* :dirty) :color))
	(if *selected_open_node* (undef (. *selected_open_node* :dirty) :color))
	(when file
		(setq *selected_open_node* (. *open_tree* :find_node file))
		(setq *selected_file_node* (. *file_tree* :find_node file))
		(def (. *selected_open_node* :dirty) :color +argb_grey12)
		(def (. *selected_file_node* :dirty) :color +argb_grey12))
	(bind '(w h) (. *file_tree* :pref_size))
	(. *file_tree* :change 0 0 w h)
	(def *file_tree* :min_width w)
	(def *file_tree_scroll* :min_width w)
	(bind '(w h) (. *open_tree* :pref_size))
	(. *open_tree* :change 0 0 w h)
	(.-> *open_tree_scroll* :layout :dirty_all)
	(.-> *file_tree_scroll* :layout :dirty_all))

;import actions and bindings
(import "apps/edit/actions.inc")

(defun main ()
	(defq *cursor_x* 0 *cursor_y* 0 *anchor_x* 0 *anchor_y* 0 *scroll_x* 0 *scroll_y* 0
		*shift_select* nil *current_buffer* nil *running* t mouse_state :u)
	(load-open-files)
	(populate-file-tree)
	(populate-open-tree)
	(populate-vdu nil)
	(select-node nil)
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
								(defq now (pii-time))
								(if (< (- now then) +click_time)
									(setq click_count (inc click_count))
									(setq click_count 0))
								(setq then now mouse_state :u)
								(cond
									((= click_count 1)
										(action-select-word))
									((= click_count 2)
										(action-select-line))
									((= click_count 3)
										(action-select-paragraph))))
							(:u ;was up last time
								))))
				(refresh))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key)
					(> (getf *msg* +ev_msg_key_keycode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key) mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_option +ev_key_mod_command))))
						;call bound control/command key action
						(if (defq action (. key_map_control :find key)) (action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. key_map_shift :find key)) (action))
							((<= +char_space key +char_tilda) (action-insert key))))
					((defq action (. key_map :find key))
						;call bound key action
						(action))
					((<= +char_space key +char_tilda)
						;insert the char
						(action-insert key))))
			(t  ;gui event
				(. *window* :event *msg*)))
		;update meta data
		(bind '(*cursor_x* *cursor_y*) (. *current_buffer* :get_cursor))
		(. *meta_map* :insert *current_file*
			(list *cursor_x* *cursor_y* *anchor_x* *anchor_y*
				*scroll_x* *scroll_y* *shift_select* *current_buffer*)))
	(. *window* :hide)
	(save-open-files))
