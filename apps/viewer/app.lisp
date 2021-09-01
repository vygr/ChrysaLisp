(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/text/buffer.inc")

(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum copy paragraph block bracket_left bracket_right))

(enums +select 0
	(enum main tip))

(defq +vdu_min_width 32 +vdu_min_height 16
	+vdu_max_width 100 +vdu_max_height 48
	*current_buffer* (Buffer) *meta_map* (fmap 31) *underlay* (list)
	*current_file* nil *selected_file_node* nil +margin 2
	+selected (apply nums (map (lambda (_)
		(const (<< (canvas-from-argb32 +argb_grey6 15) 48))) (str-alloc 8192)))
	+not_selected (nums-sub +selected +selected)
	+bracket_char (nums 0x7f) +not_whole_word_chars " .,;'`(){}[]/")

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe9ca 0xe90d 0xe955 0xe93c 0xe93d) +event_copy))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-scroll *file_tree_scroll* +scroll_flag_vertical nil
				(. (ui-tree *file_tree* +event_file_folder_action
						(:min_width 0 :color +argb_white :font *env_medium_terminal_font*))
					:connect +event_tree_action))
			(ui-backdrop _ (:color +argb_white)))
		(ui-flow _ (:flow_flags +flow_left_fill)
			(. (ui-slider *yslider*) :connect +event_yscroll)
			(ui-flow _ (:flow_flags +flow_up_fill)
				(. (ui-slider *xslider*) :connect +event_xscroll)
				(ui-flow _ (:flow_flags +flow_stack_fill :font *env_terminal_font*)
					(ui-vdu *vdu* (:min_width +vdu_min_width :min_height +vdu_min_height
							:vdu_width +vdu_max_width :vdu_height +vdu_max_height
							:ink_color +argb_white))
					(ui-vdu *vdu_underlay* (:vdu_width +vdu_max_width :vdu_height +vdu_max_height
							:min_width 0 :min_height 0 :font (get :font *vdu*)
							:ink_color (get :ink_color *vdu*))))))))

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
		(create-brackets) (create-selection))
	(. *vdu_underlay* :load *underlay* *scroll_x* *scroll_y* -1 -1))

(defun set-sliders ()
	;set slider values for current file
	(bind '(x y ax ay sx sy) (. *meta_map* :find *current_file*))
	(bind '(w h) (. *current_buffer* :get_size))
	(bind '(vw vh) (. *vdu* :vdu_size))
	(defq smaxx (max 0 (- w vw -1))
		smaxy (max 0 (- h vh -1))
		sx (max 0 (min sx smaxx)) sy (max 0 (min sy smaxy)))
	(def (. *xslider* :dirty) :maximum smaxx :portion vw :value sx)
	(def (. *yslider* :dirty) :maximum smaxy :portion vh :value sy)
	(. *meta_map* :insert *current_file* (list x y ax ay sx sy))
	(setq *scroll_x* sx *scroll_y* sy))

(defun refresh ()
	;refresh display and ensure cursor is visible
	(bind '(x y ax ay sx sy) (. *meta_map* :find *current_file*))
	(bind '(x y) (. *current_buffer* :get_cursor))
	(bind '(w h) (. *vdu* :vdu_size))
	(if (< (- x +margin) sx) (setq sx (- x +margin)))
	(if (< (- y +margin) sy) (setq sy (- y +margin)))
	(if (>= (+ x +margin) (+ sx w)) (setq sx (- (+ x +margin) w -1)))
	(if (>= (+ y +margin) (+ sy h)) (setq sy (- (+ y +margin) h -1)))
	(. *meta_map* :insert *current_file* (list x y ax ay sx sy))
	(set-sliders) (load-display))

(defun populate-file (file x y ax ay sx sy)
	;create new file buffer
	(unless (. *meta_map* :find file)
		(. *meta_map* :insert file (list x y ax ay sx sy)))
	(when file
		(defq mode (if (or (ends-with ".md" file)
						   (ends-with ".txt" file)) t nil))
		(.-> *current_buffer* (:set_mode mode) (:file_load file))))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(populate-file file 0 0 0 0 0 0)
	(bind '(x y ax ay sx sy) (. *meta_map* :find file))
	(setq *cursor_x* x *cursor_y* y *anchor_x* ax *anchor_y* ay *current_file* file)
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
	;load up the file tree
	(defq all_src_files (sort cmp (all-src-files ".")))
	(each (# (. *file_tree* :add_route %0)) (all-dirs all_src_files))
	(each (# (. *file_tree* :add_route %0)) all_src_files))

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

(defun tooltips ()
	(def *window* :tip_mbox (elem +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("copy" "select paragraph" "select form" "start form" "end form")))

;import actions and bindings
(import "./actions.inc")

(defun main ()
	(defq select (alloc-select +select_size)
		*cursor_x* 0 *cursor_y* 0 *anchor_x* 0 *anchor_y* 0 *scroll_x* 0 *scroll_y* 0
		*running* t mouse_state :u)
	(populate-file-tree)
	(populate-vdu nil)
	(select-node nil)
	(tooltips)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(action-maximise)
	(refresh)
	(while *running*
		(defq *msg* (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
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
				(bind '(x y ax ay sx sy) (. *meta_map* :find *current_file*))
				(setq sx (+ *scroll_x* (getf *msg* +ev_msg_wheel_x))
					sy (- *scroll_y* (getf *msg* +ev_msg_wheel_y)))
				(. *meta_map* :insert *current_file* (list x y ax ay sx sy))
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
						(if (defq action (. key_map_control :find key))
							(action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action
						(if (defq action (. key_map_shift :find key))
							(action)))
					((defq action (. key_map :find key))
						;call bound key action
						(action))))
			(t  ;gui event
				(. *window* :event *msg*)))
		;update meta data
		(bind '(*cursor_x* *cursor_y*) (. *current_buffer* :get_cursor))
		(. *meta_map* :insert *current_file*
			(list *cursor_x* *cursor_y* *anchor_x* *anchor_y* *scroll_x* *scroll_y*)))
	(free-select select)
	(gui-sub *window*))
