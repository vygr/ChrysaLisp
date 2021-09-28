;(import "lib/debug/frames.inc")
;(import "lib/debug/profile.inc")

(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/consts/chars.inc")
(import "lib/text/buffer.inc")
(import "lib/text/dictionary.inc")
(import "././clipboard/app.inc")

(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum open_folder_action open_leaf_action)
	(enum undo redo rewind
		cut copy paste
		reflow paragraph tab_left tab_right
		block bracket_left bracket_right
		toupper tolower ordered unique
		comment)
	(enum prev next scratch close_buffer save_all save new)
	(enum find_down find_up whole_words)
	(enum replace replace_all)
	(enum macro_playback macro_to_eof macro_record))

(enums +select 0
	(enum main tip))

(defq +vdu_min_width 80 +vdu_min_height 40 +vdu_max_width 100 +vdu_max_height 46
	+vdu_line_width 5 *current_file* nil *selected_file_node* nil
	*selected_open_node* nil *meta_map* (fmap) *open_files* (list)
	*syntax* (Syntax) *whole_words* nil *macro_record* nil *macro_actions* (list)
	+min_word_size 3 +max_matches 20 dictionary (Dictionary 1021) +margin 2
	match_window nil match_flow nil match_index -1
	+state_filename "editor_open_files" +not_whole_word_chars " .,;'`(){}[]/")

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "Edit" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe9fe 0xe99d 0xe9ff
				0xea08 0xe9ca 0xe9c9
				0xe909 0xe90d 0xe90a 0xe90b
				0xe955 0xe93c 0xe93d
				0xea36 0xea33 0xea27 0xea28
				0xe9c4) +event_undo))
		(ui-tool-bar macro_toolbar (:color (const *env_toolbar2_col*))
			(ui-buttons (0xe95e 0xe95a 0xe95f) +event_macro_playback))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar buffer_toolbar (:color (get :color macro_toolbar))
			(ui-buttons (0xe91d 0xe91e 0xe94b 0xe929 0xe97e 0xea07 0xe9f0) +event_prev))
		(ui-grid _ (:grid_width 3 :grid_height 1)
			(. (ui-textfield *name_text* (:color +argb_white
					:hint_text "new file" :clear_text "")) :connect +event_new)
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar find_toolbar (:color (get :color macro_toolbar))
					(ui-buttons (0xe914 0xe91b 0xe9cd) +event_find_down))
				(. (ui-textfield *find_text* (:color +argb_white
						:hint_text "find" :clear_text "")) :connect +event_find_down))
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar replace_toolbar (:color (get :color macro_toolbar))
					(ui-buttons (0xe95c 0xe95a) +event_replace))
				(. (ui-textfield *replace_text* (:color +argb_white
						:hint_text "replace" :clear_text "")) :connect +event_replace))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-grid _ (:color +argb_grey14 :grid_width 1 :grid_height 2)
				(ui-flow _ (:flow_flags +flow_down_fill)
					(ui-label _ (:text "Open"))
					(ui-scroll *open_tree_scroll* +scroll_flag_vertical nil
						(. (ui-tree *open_tree* +event_open_folder_action
								(:min_width 0 :color +argb_white
								:font *env_medium_terminal_font*)) :connect +event_tree_action)))
				(ui-flow _  (:flow_flags +flow_down_fill)
					(ui-label _ (:text "Project"))
					(ui-scroll *file_tree_scroll* +scroll_flag_vertical nil
						(. (ui-tree *file_tree* +event_file_folder_action
								(:min_width 0 :color (get :color *open_tree*)
								:font (get :font *open_tree*))) :connect +event_tree_action))))
			(ui-backdrop _ (:color +argb_white)))
		(ui-vdu *vdu_lines* (:min_width +vdu_line_width :min_height 0
				:vdu_width +vdu_line_width :vdu_height +vdu_min_height
				:ink_color +argb_grey12))
		(ui-backdrop _ (:color (get :ink_color *vdu_lines*) :min_width 1))
		(ui-flow _ (:flow_flags +flow_left_fill)
			(. (ui-slider *yslider*) :connect +event_yscroll)
			(ui-flow main_flow (:flow_flags +flow_up_fill)
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
					(t  ;dir
						(unless (starts-with "." file)
							(push stack (cat root "/" file))))))
				(unzip (split (pii-dirlist root) ",") (list (list) (list))))))
	files)

(defun load-display ()
	;load the vdu widgets with the text, selection and line numbers
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(defq lines (clear '()) start_line sy
		end_line (inc (min
			(elem 1 (. buffer :get_size))
			(+ start_line (get :vdu_height *edit*)))))
	(while (< (setq start_line (inc start_line)) end_line)
		(push lines (pad (str start_line) (const (dec +vdu_line_width)) "    ")))
	(. *vdu_lines* :load lines 0 0 -1 -1)
	(. buffer :vdu_load (. *edit* :get_vdu_text) sx sy)
	(if (and (= cx ax) (= cy ay))
		(. *edit* :underlay_brackets)
		(. *edit* :underlay_selection)))

(defun set-sliders ()
	;set slider values for current file
	(bind '(cx cy ax ay sx sy _ buffer) (. *meta_map* :find *current_file*))
	(bind '(w h) (. buffer :get_size))
	(bind '(vw vh) (. (. *edit* :get_vdu_text) :vdu_size))
	(defq smaxx (max 0 (- w vw -1)) smaxy (max 0 (- h vh -1))
		sx (max 0 (min sx smaxx)) sy (max 0 (min sy smaxy)))
	(def (. *xslider* :dirty) :maximum smaxx :portion vw :value sx)
	(def (. *yslider* :dirty) :maximum smaxy :portion vh :value sy)
	(. *meta_map* :insert *current_file* (list cx cy ax ay sx sy _ buffer))
	(. *edit* :set_scroll sx sy))

(defun refresh ()
	(unless (get :macro_playback)
		;refresh display and ensure cursor is visible
		(bind '(cx cy ax ay sx sy _ buffer) (. *meta_map* :find *current_file*))
		(bind '(cx cy) (. buffer :get_cursor))
		(bind '(w h) (. (. *edit* :get_vdu_text) :vdu_size))
		(if (< (- cx +margin) sx) (setq sx (- cx +margin)))
		(if (< (- cy +margin) sy) (setq sy (- cy +margin)))
		(if (>= (+ cx +margin) (+ sx w)) (setq sx (- (+ cx +margin) w -1)))
		(if (>= (+ cy +margin) (+ sy h)) (setq sy (- (+ cy +margin) h -1)))
		(. *meta_map* :insert *current_file* (list cx cy ax ay sx sy _ buffer))
		(set-sliders) (load-display)))

(defun populate-dictionary (line)
	;populate dictionary with this lines words
	(task-slice)
	(each (lambda (word)
			(if (>= (length word) +min_word_size)
				(. dictionary :insert_word word)))
		(split line +not_whole_word_chars)))

(defun populate-file (file x y ax ay sx sy)
	;create new file buffer
	(unless (. *meta_map* :find file)
		(defq mode (if (or (ends-with ".md" file)
						   (ends-with ".txt" file)) t nil))
		(. *meta_map* :insert file
			(list x y ax ay sx sy nil (defq buffer (Buffer mode *syntax*))))
		(when file
			(. buffer :file_load file)
			(unless (find file *open_files*) (push *open_files* file))
			(each populate-dictionary (. buffer :get_text_lines)))))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(populate-file file 0 0 0 0 0 0)
	(bind '(cx cy ax ay sx sy _ buffer) (. *meta_map* :find file))
	(setq *current_file* file)
	(. *edit* :set_buffer buffer)
	(. *edit* :set_cursor cx cy)
	(. *edit* :set_anchor ax ay)
	(. *edit* :set_scroll sx sy)
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
	(each (# (. *open_tree* :add_route %0)) (defq dirs (all-dirs *open_files*)))
	(each (# (. *open_tree* :add_route %0)) *open_files*)
	(each (# (. *file_tree* :add_route %0)) dirs)
	(each (# (. *file_tree* :add_route %0)) *open_files*))

(defun load-open-files ()
	;load users open file tree
	(when (defq stream (file-stream (cat *env_home* +state_filename)))
		(each-line (lambda (line)
				(bind '(form _) (read (string-stream line) +char_space))
				(bind '(file (x y ax ay sx sy _)) form)
				(if (/= (age file) 0) (populate-file file x y ax ay sx sy)))
			stream)))

(defun save-open-files ()
	;save users open file tree
	(when (defq stream (file-stream (cat *env_home* +state_filename) +file_open_write))
		(each (lambda (file)
				(write-line stream (str (list file (slice 0 -2 (. *meta_map* :find file))))))
			(sort cmp *open_files*))))

(defun window-resize ()
	;layout the window and size the vdu to fit
	(bind '(w h) (. (. *edit* :get_vdu_text) :max_size))
	(set *edit* :vdu_width w :vdu_height h)
	(set *vdu_lines* :vdu_height h)
	(. *edit* :layout)
	(. *vdu_lines* :layout)
	(set-sliders) (load-display))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(set *edit* :vdu_width w :vdu_height h :min_width w :min_height h)
	(set *vdu_lines* :vdu_height h :min_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(set *edit* :min_width +vdu_min_width :min_height +vdu_min_height)
	(set *vdu_lines* :min_height +vdu_min_height)
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
	(def *open_tree* :min_width w)
	(def *open_tree_scroll* :min_width w)
	(bind '(w h) (. *open_tree* :pref_size))
	(. *open_tree* :change 0 0 w h)
	(.-> *open_tree_scroll* :layout :dirty_all)
	(.-> *file_tree_scroll* :layout :dirty_all))

(defun tooltips ()
	(def *window* :tip_mbox (elem +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("undo" "redo" "rewind" "cut" "copy" "paste" "reflow" "select paragraph"
		"outdent" "indent" "select form" "start form" "end form" "upper case"
		"lower case" "sort" "unique" "comment"))
	(each (# (def %0 :tip_text %1)) (. buffer_toolbar :children)
		'("previous" "next" "scratchpad" "close" "save all" "save" "new"))
	(each (# (def %0 :tip_text %1)) (. find_toolbar :children)
		'("find down" "find up" "whole words"))
	(each (# (def %0 :tip_text %1)) (. macro_toolbar :children)
		'("playback" "playback eof" "record"))
	(each (# (def %0 :tip_text %1)) (. replace_toolbar :children)
		'("replace" "replace all")))

(defun clear-matches ()
	(if match_window (gui-sub match_window))
	(setq match_window nil match_flow nil match_index -1))

(defun show-matches ()
	(clear-matches)
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(bind '(x x1) (select-word))
	(when (>= (- x1 x) +min_word_size)
		(defq match_words (. dictionary :find_matches_case
			(slice x x1 (. buffer :get_text_line cy))))
		(when (> (length match_words) 0)
			(if (> (length match_words) +max_matches)
				(setq match_words (slice 0 +max_matches match_words)))
			(ui-window window (:color (get :color *window*)
					:ink_color (get :ink_color *edit*) :font (get :font *edit*))
				(ui-flow flow (:flow_flags +flow_down_fill)
					(each (# (ui-label _ (:text %0))) match_words)))
			(bind '(cw ch) (. (. *edit* :get_vdu_text) :char_size))
			(defq x (+ (getf *edit* +view_ctx_x 0) (- (* cx cw) (* sx cw)))
				y (+ (getf *edit* +view_ctx_y 0) (- (* (inc cy) ch) (* sy ch))))
			(bind '(w h) (.-> window (:set_flags 0 +view_flag_solid) :pref_size))
			(bind '(cx cy sw sh) (gui-info))
			(if (> (+ x w) sw) (setq x (+ (- x w) cw)))
			(if (> (+ y h) sh) (setq y (- y h ch)))
			(. window :change x y w h)
			(gui-add-front (setq match_flow flow match_window window)))))

(defun select-match (dir)
	(when match_window
		(defq matches (. match_flow :children))
		(if (>= match_index 0) (undef (. (elem match_index matches) :dirty) :color))
		(setq match_index (+ match_index dir))
		(if (< match_index 0) (setq match_index (dec (length matches))))
		(if (> match_index (dec (length matches))) (setq match_index 0))
		(def (. (elem match_index matches) :dirty) :color +argb_red)))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(and *macro_record* (find (elem 0 action) recorded_actions_list)
		(push *macro_actions* action))
	(catch (eval action) (progn (print _)(print) t)))

(defun main ()
	(defq select (alloc-select +select_size)
		edit_service (mail-declare (task-mailbox) "EDIT_SERVICE" "Edit Service 0.1")
		*running* t *edit* (Editor-edit))
	(.-> *edit* (:set_buffer (Buffer)) (:set_underlay_color +argb_grey6))
	(. main_flow :add_back *edit*)
	;load up the base Syntax keywords and boot.inc words for matching
	(each (lambda ((key val)) (. dictionary :insert_word (str key)))
		(tolist (get :keywords *syntax* )))
	(each-line populate-dictionary (file-stream "class/lisp/boot.inc"))
	(populate-file-tree)
	(load-open-files)
	(populate-open-tree)
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
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key)
					(> (getf *msg* +ev_msg_key_keycode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key) mod (getf *msg* +ev_msg_key_mod))
				(cond
					((and match_window (or (= key 0x40000052) (= key 0x40000051)
							(and (or (= key +char_lf) (= key +char_cr) (= key +char_space))
								(>= match_index 0))))
						;matches navigation and selection
						(cond
							((or (= key +char_lf) (= key +char_cr) (= key +char_space))
								;choose a match
								(defq word (get :text (elem match_index (. match_flow :children))))
								(if (= key +char_space) (setq word (cat word " ")))
								(clear-matches)
								(dispatch-action action-select-word)
								(dispatch-action action-insert word))
							((select-match (if (= key 0x40000052) -1 1)))))
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_option +ev_key_mod_command))))
						;call bound control/command key action
						(when (defq action (. key_map_control :find key))
							(clear-matches)
							(dispatch-action action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. key_map_shift :find key))
								(clear-matches)
								(dispatch-action action))
							((<= +char_space key +char_tilda)
								(dispatch-action action-insert (char key))
								(show-matches))))
					((defq action (. key_map :find key))
						;call bound key action
						(clear-matches)
						(dispatch-action action))
					((<= +char_space key +char_tilda)
						;insert the char
						(dispatch-action action-insert (char key))
						(show-matches))))
			(t  ;gui event
				(clear-matches)
				(. *window* :event *msg*)))
		;update meta data
		(defq buffer (. *edit* :get_buffer))
		(bind '(cx cy) (. *edit* :get_cursor))
		(bind '(ax ay) (. *edit* :get_anchor))
		(bind '(sx sy) (. *edit* :get_scroll))
		(. *meta_map* :insert *current_file* (list cx cy ax ay sx sy nil buffer)))
	(free-select select)
	(clear-matches)
	(gui-sub *window*)
	(action-save-all)
	(save-open-files)
	(mail-forget edit_service))
