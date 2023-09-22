;; (import "lib/debug/frames.inc")
;; (import "lib/debug/profile.inc")

(import "././login/env.inc")
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/consts/chars.inc")
(import "lib/text/buffer.inc")
(import "lib/text/dictionary.inc")
(import "lib/task/local.inc")
(import "././clipboard/app.inc")

(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum open_folder_action open_leaf_action)
	(enum undo redo rewind gundo gredo
		cut copy paste
		reflow paragraph tab_left tab_right
		block bracket_left bracket_right
		toupper tolower sort unique
		invert comment)
	(enum prev next scratch close_buffer close_all save save_all load_all new)
	(enum global region whole_words regexp find_down find_up)
	(enum replace replace_all replace_global)
	(enum macro_playback macro_to_eof macro_global macro_record)
	(enum open_tree_collapse open_tree_expand)
	(enum file_tree_collapse file_tree_expand))

(enums +select 0
	(enum main tip))

(bind '(+edit_font +edit_size) (font-info *env_editor_font*))

(defq +vdu_min_width 40 +vdu_min_height 20 +vdu_max_width 120 +vdu_max_height 80
	+vdu_line_width 5 +min_word_size 3 +max_matches 20 +margin 2
	+state_filename "editor_state" +not_whole_word_chars (cat " .,;'`(){}[]/" (ascii-char 34))
	+text_types ''(".md" ".txt") +file_types ''(".lisp" ".inc" ".vp" ".md" ".txt"))

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "Edit" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar ()
			(ui-buttons (0xe9fe 0xe99d 0xe9ff 0xe938 0xe971
				0xea08 0xe9ca 0xe9c9
				0xe909 0xe90d 0xe90a 0xe90b
				0xe955 0xe93c 0xe93d
				0xea36 0xea33 0xea27 0xea28
				0xea26 0xe9c4) +event_undo))
		(ui-tool-bar macro_toolbar (:color (const *env_toolbar2_col*))
			(ui-buttons (0xe95c 0xe95e 0xe95a 0xe95f) +event_macro_playback))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar buffer_toolbar (:color (get :color macro_toolbar))
			(ui-buttons (0xe91d 0xe91e 0xe94b 0xe94d 0xe94c 0xea07 0xe97e 0xea3d 0xe94e) +event_prev))
		(ui-grid _ (:grid_height 1)
			(. (ui-textfield *name_text* (:color +argb_white
					:hint_text "new file" :clear_text "")) :connect +event_new)
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar find_toolbar (:color (get :color macro_toolbar))
					(ui-buttons (0xe9a1 0xe962 0xe9cd 0xe9a8 0xe914 0xe91b) +event_global))
				(. (ui-textfield *find_text* (:color +argb_white
						:hint_text "find" :clear_text "")) :connect +event_find_down))
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar replace_toolbar (:color (get :color macro_toolbar))
					(ui-buttons (0xe95c 0xe95e 0xe95a) +event_replace))
				(. (ui-textfield *replace_text* (:color +argb_white
						:hint_text "replace" :clear_text "")) :connect +event_replace))))
	(ui-flow *edit_flow* (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-grid _ (:color +argb_grey14 :grid_width 1)
				(ui-flow _ (:flow_flags +flow_down_fill)
					(ui-flow _ (:flow_flags +flow_left_fill)
						(ui-buttons (">" "^") +event_open_tree_collapse)
						(ui-label _ (:text "Open")))
					(ui-scroll *open_tree_scroll* +scroll_flag_vertical :nil
						(. (ui-tree *open_tree* +event_open_folder_action
								(:min_width 0 :color +argb_white
								:font *env_medium_terminal_font*)) :connect +event_tree_action)))
				(ui-flow _  (:flow_flags +flow_down_fill)
					(ui-flow _ (:flow_flags +flow_left_fill)
						(ui-buttons (">" "^") +event_file_tree_collapse)
						(ui-label _ (:text "Project")))
					(ui-scroll *file_tree_scroll* +scroll_flag_vertical :nil
						(. (ui-tree *file_tree* +event_file_folder_action
								(:min_width 0 :color (get :color *open_tree*)
								:font (get :font *open_tree*))) :connect +event_tree_action))))
			(ui-backdrop _ (:color +argb_white)))
		(ui-vdu *vdu_lines* (:min_width +vdu_line_width :min_height 0
				:vdu_width +vdu_line_width :vdu_height +vdu_min_height
				:ink_color +argb_grey12 :font *env_editor_font*))
		(ui-backdrop _ (:color (get :ink_color *vdu_lines*) :min_width 1))
		(ui-flow _ (:flow_flags +flow_left_fill)
			(. (ui-slider *yslider*) :connect +event_yscroll)
			(ui-flow *main_flow* (:flow_flags +flow_up_fill)
				(. (ui-slider *xslider*) :connect +event_xscroll)))))

(defun radio-select (toolbar states)
	(each (# (undef (. %0 :dirty) :color)
			(if %1 (def %0 :color *env_radio_col*)))
		(. toolbar :children) states))

(defun refresh-display ()
	;load the vdu widgets with the text, selection and line numbers
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(defq lines (clear '()) start_line sy
		end_line (inc (min
			(elem-get 1 (. buffer :get_size))
			(+ start_line (get :vdu_height *edit*)))))
	(while (< (setq start_line (inc start_line)) end_line)
		(push lines (pad (str start_line) (const (dec +vdu_line_width)) "    ")))
	(. *vdu_lines* :load lines 0 0 -1 -1)
	(. buffer :vdu_load (. *edit* :get_vdu_text) sx sy)
	(. *edit* :underlay_find)
	(if (and (= cx ax) (= cy ay))
		(. *edit* :underlay_brackets)
		(. *edit* :underlay_selection)))

(defun refresh-sliders ()
	;set slider values for current file
	(defq meta (.-> *meta_map* (:find :files) (:find (str *current_file*))))
	(bind '(sx sy buffer) (gather meta :sx :sy :buffer))
	(bind '(w h) (. buffer :get_size))
	(bind '(vw vh) (.-> *edit* :get_vdu_text :vdu_size))
	(defq smaxx (max 0 (- w vw -1)) smaxy (max 0 (- h vh -1))
		sx (max 0 (min sx smaxx)) sy (max 0 (min sy smaxy)))
	(def (. *xslider* :dirty) :maximum smaxx :portion vw :value sx)
	(def (. *yslider* :dirty) :maximum smaxy :portion vh :value sy)
	(scatter meta :sx sx :sy sy)
	(. *edit* :set_scroll sx sy))

(defun refresh ()
	(when (<= (last *refresh_mode*) 0)
		;refresh display and ensure cursor is visible
		(defq meta (.-> *meta_map* (:find :files) (:find (str *current_file*))))
		(bind '(sx sy buffer) (gather meta :sx :sy :buffer))
		(bind '(cx cy) (. buffer :get_cursor))
		(bind '(w h) (.-> *edit* :get_vdu_text :vdu_size))
		(if (< (- cx +margin) sx) (setq sx (- cx +margin)))
		(if (< (- cy +margin) sy) (setq sy (- cy +margin)))
		(if (>= (+ cx +margin) (+ sx w)) (setq sx (- (+ cx +margin) w -1)))
		(if (>= (+ cy +margin) (+ sy h)) (setq sy (- (+ cy +margin) h -1)))
		(scatter meta :sx sx :sy sy)
		(refresh-sliders) (refresh-display)))

(defun populate-dictionary (line)
	;populate dictionary with this lines words
	(task-slice)
	(each (lambda (word)
			(if (>= (length word) +min_word_size)
				(. dictionary :insert_word word)))
		(split line +not_whole_word_chars)))

(defun populate-buffer (file cx cy ax ay sx sy)
	;create new file buffer ?
	(defq mode (if (some (# (ends-with %0 file)) +text_types) :t :nil)
		files (. *meta_map* :find :files) key (str file)
		meta (. files :find key))
	(unless meta
		(. files :insert key (setq meta
			(Fmap-kv :cx cx :cy cy :ax ax :ay ay :sx sx :sy sy :buffer :nil))))
	(unless (defq buffer (. meta :find :buffer))
		(. meta :insert :buffer (setq buffer (Buffer mode *syntax*)))
		(when file
			(. buffer :file_load file)
			(each populate-dictionary (. buffer :get_text_lines)))))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(populate-buffer file 0 0 0 0 0 0)
	(defq meta (.-> *meta_map* (:find :files) (:find (str file))))
	(bind '(cx cy ax ay sx sy buffer) (gather meta :cx :cy :ax :ay :sx :sy :buffer))
	(setq *current_file* file)
	(bind '(cx cy) (. buffer :constrain cx cy))
	(bind '(ax ay) (. buffer :constrain ax ay))
	(bind '(sx sy) (. buffer :constrain sx sy))
	(bind '(fx fy fx1 fy1) '(0 0 0 0))
	(.-> *edit* (:set_buffer buffer)
		(:set_cursor cx cy)
		(:set_anchor ax ay)
		(:set_find fx fy fx1 fy1)
		(:set_scroll sx sy))
	(scatter meta :cx cx :cy cy :ax ax :ay ay :sx sx :sy sy)
	(radio-select find_toolbar (list :nil :nil *whole_words* *regexp* :nil :nil))
	(def *title* :text (cat "Edit -> " (if file file "<scratch pad>")))
	(.-> *title* :layout :dirty)
	(refresh))

(defun populate-file-trees ()
	;reload open tree
	(sort cmp *open_files*)
	(. *open_tree* :empty)
	(each (# (. *open_tree* :add_route %0)) (defq dirs (all-dirs *open_files*)))
	(each (# (. *open_tree* :add_route %0)) *open_files*)
	(each (# (. *file_tree* :add_route %0)) dirs)
	(each (# (. *file_tree* :add_route %0)) *open_files*)
	(bind '(w h) (. *file_tree* :pref_size))
	(def *file_tree* :min_width w)
	(def *open_tree* :min_width w)
	(def *file_tree_scroll* :min_width w)
	(def *open_tree_scroll* :min_width w)
	(.-> *file_tree* (:change 0 0 w h) :layout)
	(bind '(w h) (. *open_tree* :pref_size))
	(.-> *open_tree* (:change 0 0 w h) :layout)
	(.-> *open_tree_scroll* :layout :dirty_all)
	(.-> *file_tree_scroll* :layout :dirty_all))

(defun window-resize ()
	;layout the window and size the vdu to fit
	(bind '(w h) (. *edit* :max_size))
	(set *edit* :vdu_width w :vdu_height h)
	(set *vdu_lines* :vdu_height h)
	(. *edit* :layout)
	(. *vdu_lines* :layout)
	(refresh-sliders) (refresh-display))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(set *edit* :vdu_width w :vdu_height h :min_width w :min_height h)
	(set *vdu_lines* :vdu_height h :min_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(set *edit* :min_width +vdu_min_width :min_height +vdu_min_height)
	(set *vdu_lines* :min_height +vdu_min_height)
	(. *window* :change_dirty x y w h)
	(window-resize))

(defun visible-node (tree file)
	;highlight and show the selected file
	(. tree :select file)
	(bind '(_ y _ h) (. tree :get_relative file))
	(defq tree (penv tree) scroll (get :vslider tree))
	(bind '(_ th) (. tree :get_size))
	(defq val (get :value scroll))
	(if (< y val) (def scroll :value y))
	(if (> (+ y h) (+ val th)) (def scroll :value (- (+ y h) th)))
	(.-> tree :layout :dirty_all))

(defun select-node (file)
	;highlight and show the selected file in both tree views
	(visible-node *open_tree* file)
	(visible-node *file_tree* file))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip select))
	(ui-tool-tips main_toolbar
		'("undo" "redo" "rewind" "global undo" "global redo"
		"cut" "copy" "paste" "reflow" "select paragraph"
		"outdent" "indent" "select form" "start form"
		"end form" "upper case" "lower case" "sort" "unique"
		"reverse" "comment"))
	(ui-tool-tips buffer_toolbar
		'("previous" "next" "scratchpad" "close" "close all"
		"save" "save all" "load all" "new"))
	(ui-tool-tips find_toolbar
		'("global search" "select region" "whole words"
		"regexp" "find down" "find up"))
	(ui-tool-tips macro_toolbar
		'("playback" "playback eof" "playback global" "record"))
	(ui-tool-tips replace_toolbar
		'("replace" "replace all" "replace global")))

(defun clear-matches ()
	(if match_window (gui-sub match_window))
	(setq match_window :nil match_flow :nil match_index -1))

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
			(bind '(cw ch) (.-> *edit* :get_vdu_text :char_size))
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
		(if (>= match_index 0) (undef (. (elem-get match_index matches) :dirty) :color))
		(setq match_index (+ match_index dir))
		(if (< match_index 0) (setq match_index (dec (length matches))))
		(if (> match_index (dec (length matches))) (setq match_index 0))
		(def (. (elem-get match_index matches) :dirty) :color +argb_red)))

(defun page-scale (s)
	(n2i (* (n2f s) *page_scale*)))

(defun update-meta-data ()
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(scatter *meta_map*
		:file (str *current_file*))
	(scatter (.-> *meta_map* (:find :files) (:find (str *current_file*)))
		:cx cx :cy cy :ax ax :ay ay :sx sx :sy sy :buffer buffer))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(defq func (first action))
	(if (find func *find_actions*)
		(push action *whole_words* *regexp* (. *find_text* :get_text)))
	(if (find func *replace_actions*)
		(push action (. *replace_text* :get_text)))
	(and *macro_record* (find func *recorded_actions*)
		(macro-record action))
	(catch (eval action)
		(progn (print _)(print)
			(setq *refresh_mode* (list 0)) :t)))

(defun main ()
	(defq select (alloc-select +select_size)
		edit_service (mail-declare (task-netid) "Edit" "Edit Service 0.1")
		*running* :t *edit* (Editor-edit) *page_scale* 1.0 *regexp* :nil
		*syntax* (Syntax) *whole_words* :nil *refresh_mode* (list 0)
		*macro_record* :nil *macro_actions* (list)
		dictionary (Dictionary 1031) match_window :nil match_flow :nil match_index -1
		*meta_map* :nil *open_files* :nil *current_file* (state-load))
	(.-> *edit* (:set_buffer (Buffer))
		(:set_underlay_color +argb_grey6)
		(:set_underlay_find_color +argb_grey3))
	(def *edit* :min_width 0 :min_height 0
		:vdu_width +vdu_min_width :vdu_height +vdu_min_height)
	(. *main_flow* :add_back *edit*)
	;load up the base Syntax keywords and root.inc and english words for matching
	(each (lambda ((key val)) (. dictionary :insert_word (str key)))
		(tolist (get :keywords *syntax* )))
	(each-line populate-dictionary (file-stream "class/lisp/root.inc"))
	(each-line populate-dictionary (file-stream "lib/text/english.txt"))
	(. *file_tree* :populate "." +file_types 2)
	(populate-file-trees)
	(populate-vdu *current_file*)
	(tooltips)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(action-maximise)
	(select-node *current_file*)
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
				(dispatch-action action))
			((= (getf *msg* +ev_msg_type) +ev_type_key_up)
				;key up event
				)
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key down event
				(defq key (getf *msg* +ev_msg_key_key) mod (getf *msg* +ev_msg_key_mod))
				(cond
					((and match_window (or (= key 0x40000052) (= key 0x40000051)
							(and (or (= key +char_lf) (= key +char_cr) (= key +char_space))
								(>= match_index 0))))
						;matches navigation and selection
						(cond
							((or (= key +char_lf) (= key +char_cr) (= key +char_space))
								;choose a match
								(defq word (get :text (elem-get match_index (. match_flow :children))))
								(if (= key +char_space) (setq word (cat word " ")))
								(clear-matches)
								(dispatch-action action-select-word)
								(dispatch-action action-insert word))
							((select-match (if (= key 0x40000052) -1 1)))))
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta))))
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
							((<= +char_space key +char_tilde)
								(dispatch-action action-insert (char key))
								(show-matches))))
					((defq action (. key_map :find key))
						;call bound key action
						(clear-matches)
						(dispatch-action action))
					((<= +char_space key +char_tilde)
						;insert the char
						(dispatch-action action-insert (char key))
						(show-matches))))
			(:t ;gui event
				(clear-matches)
				(. *window* :event *msg*)))
		;update meta data
		(update-meta-data))
;;     (profile-report "Editor")
	(action-save-all)
	(free-select select)
	(clear-matches)
	(gui-sub *window*)
	(mail-forget edit_service))
