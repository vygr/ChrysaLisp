;(import "lib/debug/frames.inc")

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
	*meta_map* (fmap 31) *current_file* :nil *selected_file_node* :nil +margin 2)

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe9ca 0xe90d 0xe955 0xe93c 0xe93d) +event_copy))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-scroll *file_tree_scroll* +scroll_flag_vertical :nil
				(. (ui-tree *file_tree* +event_file_folder_action
						(:min_width 0 :color +argb_white :font *env_medium_terminal_font*))
					:connect +event_tree_action))
			(ui-backdrop _ (:color +argb_white)))
		(ui-flow _ (:flow_flags +flow_left_fill)
			(. (ui-slider *yslider*) :connect +event_yscroll)
			(ui-flow *main_flow* (:flow_flags +flow_up_fill)
				(. (ui-slider *xslider*) :connect +event_xscroll)))))

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
					(:t  ;dir
						(unless (starts-with "." file)
							(push stack (cat root "/" file))))))
				(unzip (split (pii-dirlist root) ",") (list (list) (list))))))
	files)

(defun load-display ()
	;load the vdu widgets with the text, selection and line numbers
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(.-> *edit* :get_buffer (:vdu_load (. *edit* :get_vdu_text) sx sy))
	(if (and (= cx ax) (= cy ay))
		(. *edit* :underlay_brackets)
		(. *edit* :underlay_selection)))

(defun set-sliders ()
	;set slider values for current file
	(bind '(cx cy ax ay sx sy) (. *meta_map* :find *current_file*))
	(bind '(w h) (.-> *edit* :get_buffer :get_size))
	(bind '(vw vh) (.-> *edit* :get_vdu_text :vdu_size))
	(defq smaxx (max 0 (- w vw -1)) smaxy (max 0 (- h vh -1))
		sx (max 0 (min sx smaxx)) sy (max 0 (min sy smaxy)))
	(def (. *xslider* :dirty) :maximum smaxx :portion vw :value sx)
	(def (. *yslider* :dirty) :maximum smaxy :portion vh :value sy)
	(. *meta_map* :insert *current_file* (list cx cy ax ay sx sy))
	(. *edit* :set_scroll sx sy))

(defun refresh ()
	;refresh display and ensure cursor is visible
	(bind '(cx cy ax ay sx sy) (. *meta_map* :find *current_file*))
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(w h) (.-> *edit* :get_vdu_text :vdu_size))
	(if (< (- cx +margin) sx) (setq sx (- cx +margin)))
	(if (< (- cy +margin) sy) (setq sy (- cy +margin)))
	(if (>= (+ cx +margin) (+ sx w)) (setq sx (- (+ cx +margin) w -1)))
	(if (>= (+ cy +margin) (+ sy h)) (setq sy (- (+ cy +margin) h -1)))
	(. *meta_map* :insert *current_file* (list cx cy ax ay sx sy))
	(set-sliders) (load-display))

(defun populate-file (file)
	;create new file buffer
	(unless (. *meta_map* :find file)
		(. *meta_map* :insert file (list 0 0 0 0 0 0)))
	(when file
		(defq mode (if (or (ends-with ".md" file)
						   (ends-with ".txt" file)) :t :nil)
			buffer (Buffer))
		(.-> buffer (:set_mode mode) (:file_load file))
		(. *edit* :set_buffer buffer)))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(populate-file file)
	(bind '(cx cy ax ay sx sy) (. *meta_map* :find file))
	(. *edit* :set_cursor cx cy)
	(. *edit* :set_anchor ax ay)
	(. *edit* :set_scroll sx sy)
	(setq *current_file* file)
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
	(bind '(w h) (. *edit* :max_size))
	(set *edit* :vdu_width w :vdu_height h)
	(. *edit* :layout)
	(set-sliders) (load-display))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(set *edit* :vdu_width w :vdu_height h :min_width w :min_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h)
	(window-resize))

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
	(def *window* :tip_mbox (elem-get +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("copy" "select paragraph" "select form" "start form" "end form")))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun main ()
	(defq select (alloc-select +select_size) *running* :t *edit* (Viewer-edit))
	(.-> *edit* (:set_buffer (Buffer)) (:set_underlay_color +argb_grey6))
	(def *edit* :min_width 0 :min_height 0
		:vdu_width +vdu_min_width :vdu_height +vdu_min_height)
	(. *main_flow* :add_back *edit*)
	(populate-file-tree)
	(populate-vdu :nil)
	(select-node :nil)
	(tooltips)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(action-maximise)
	(refresh)
	(while *running*
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
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
						(if (defq action (. key_map_control :find key))
							(action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action
						(if (defq action (. key_map_shift :find key))
							(action)))
					((defq action (. key_map :find key))
						;call bound key action
						(action))))
			(:t  ;gui event
				(. *window* :event *msg*)))
		;update meta data
		(defq buffer (. *edit* :get_buffer))
		(bind '(cx cy) (. *edit* :get_cursor))
		(bind '(ax ay) (. *edit* :get_anchor))
		(bind '(sx sy) (. *edit* :get_scroll))
		(. *meta_map* :insert *current_file* (list cx cy ax ay sx sy)))
	(free-select select)
	(gui-sub *window*))
