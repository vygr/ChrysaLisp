(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/chars.inc")
(import "lib/text/buffer.inc")
(import "lib/text/dictionary.inc")
(import "lib/task/cmd.inc")
(import "service/clipboard/app.inc")

;our UI widgets and events
(import "./widgets.inc")
(import "./rpc.inc")

(enums +select 0
	(enum main tip remote))

(bind '(+edit_font +edit_size) (font-info *env_editor_font*))

(defq +min_word_size 3 +max_matches 20 +margin 2
	+state_filename "editor.tre"
	+text_types ''(".md" ".txt")
	+file_types ''(".lisp" ".inc" ".vp" ".md" ".txt" ".tre" ".cwb")
	+dictionaries ''("lib/text/english.txt"))

(defun toolbar-states (toolbar states)
	(defq radio_col (canvas-brighter (get :color toolbar)))
	(each (# (undef (. %0 :dirty) :color)
			(if %1 (def %0 :color radio_col)))
		(. toolbar :children) states))

(defun refresh-display ()
	;load the vdu widgets with the text, selection and line numbers
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(defq lines (clear '()) start_line sy
		end_line (inc (min
			(second (. buffer :get_size))
			(+ start_line (get :vdu_height *edit*)))))
	(while (< (setq start_line (inc start_line)) end_line)
		(push lines (pad (str start_line) (const (dec +vdu_line_width)) "    ")))
	(. *vdu_lines* :load lines 0 0 -1 -1)
	(.-> buffer (:vdu_load (. *edit* :get_vdu_text) sx sy)
		(:find (. *find_text* :get_text) *whole_words* *regexp*))
	(.-> *edit* :underlay_paper :underlay_ink)
	;update status bar
	(each (# (def (. %0 :dirty) :text (str %1)))
		(list *cx* *cy* *sw* *sh* *fc*)
		(list (inc cx) (inc cy) (abs (- cx ax)) (abs (- cy ay)) (find-count))))

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
		(split line +char_class_not_whole_word)))

(defun populate-buffer (file cx cy ax ay sx sy)
	;create new file buffer ?
	(defq mode (notany (# (ends-with %0 file)) +text_types)
		files (. *meta_map* :find :files) key (str file)
		meta (. files :find key))
	(unless meta
		(. files :insert key (setq meta
			(scatter (Emap) :cx cx :cy cy :ax ax :ay ay :sx sx :sy sy :buffer :nil))))
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
	(toolbar-states *find_toolbar* (list :nil :nil *whole_words* *regexp* :nil :nil))
	(def *title* :text (cat "Edit -> " (if file file "<scratch pad>")))
	(.-> *title* :layout :dirty)
	(refresh))

(defun populate-file-trees ()
	;reload open tree
	(sort *open_files*)
	(. *open_files_selector* :empty)
	(each (# (. *open_files_selector* :add_route %0)) (defq dirs (files-dirs *open_files*)))
	(each (# (. *open_files_selector* :add_route %0)) *open_files*)
	(each (# (. *file_selector* :add_route %0)) dirs)
	(each (# (. *file_selector* :add_route %0)) *open_files*)
	(. *open_files_selector* :layout_tree))

(defun window-resize ()
	;layout the window and size the vdu to fit
	(bind '(w h) (. *edit* :max_size))
	(set *edit* :vdu_width w :vdu_height h)
	(set *vdu_lines* :vdu_height h)
	(. *edit* :constrain)
	(. *vdu_lines* :constrain)
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

(defun select-node (file)
	;highlight and show the selected file in both tree views
	(. *open_files_selector* :select_node file)
	(. *file_selector* :select_node file))

(defun clear-matches ()
	(if match_window (gui-sub-rpc match_window))
	(setq match_window :nil match_flow :nil match_index -1))

(defun show-matches ()
	(clear-matches)
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(bind '(x x1) (select-word))
	(when (>= (- x1 x) +min_word_size)
		(defq match_words (. dictionary :find_matches_case
			(slice (. buffer :get_text_line cy) x x1)))
		(when (> (length match_words) 0)
			(if (> (length match_words) +max_matches)
				(setq match_words (slice match_words 0 +max_matches)))
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
			(gui-add-front-rpc (setq match_flow flow match_window window)))))

(defun select-match (dir)
	(when match_window
		(defq matches (. match_flow :children))
		(if (>= match_index 0) (undef (. (elem-get matches match_index) :dirty) :color))
		(setq match_index (+ match_index dir))
		(if (< match_index 0) (setq match_index (dec (length matches))))
		(if (> match_index (dec (length matches))) (setq match_index 0))
		(def (. (elem-get matches match_index) :dirty) :color +argb_red)))

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
		(progn (prin _) (print)
			(setq *refresh_mode* (list 0)) :t)))

(defun main ()
	(defq select (task-mboxes +select_size)
		edit_service (mail-declare (elem-get select +select_remote) "Edit" "Edit Service 1.0")
		*running* :t *edit* (Editor-edit) *page_scale* 1.0 *regexp* :nil
		*syntax* (Syntax) *whole_words* :nil *refresh_mode* (list 0)
		*macro_record* :nil *macro_actions* (list) *cursor_stack* (list)
		dictionary (Dictionary 1031) match_window :nil match_flow :nil match_index -1
		*meta_map* :nil *open_files* :nil
		*x* 0 *y* 0 *width* 1024 *height* 512 *current_file* (state-load))
	(.-> *edit* (:set_buffer (Buffer))
		(:set_select_color +argb_grey6)
		(:set_found_color +argb_grey4)
		(:set_region_color +argb_grey3))
	(def *edit* :min_width 0 :min_height 0
		:vdu_width +vdu_min_width :vdu_height +vdu_min_height)
	(. *edit_flow* :add_back *edit*)
	(def *window* :tip_mbox (elem-get select +select_tip))
	;load up the base Syntax keywords, root.inc and dictionaries for matching
	(each (lambda ((key val)) (. dictionary :insert_word (str key)))
		(tolist (get :keywords *syntax* )))
	(each (# (lines! populate-dictionary (file-stream %0)))
		(cat +dictionaries '("class/lisp/root.inc")))
	(. *file_selector* :populate "." +file_types 2)
	(populate-file-trees)
	(populate-vdu *current_file*)
	(bind '(x y w h) (view-fit *x* *y* *width* *height*))
	(.-> *window* (:change x y w h) (:connect +event_layout))
	(window-resize)
	(gui-add-front-rpc *window*)
	(select-node *current_file*)
	(refresh)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_remote)
				;remote command message
				(case (getf *msg* +edit_rpc_type)
					(+edit_rpc_type_jump
						(mail-send (getf *msg* +edit_rpc_reply_id) "")
						(bind '(brk_id file line) (split (slice *msg* +edit_rpc_jump_names -1) "|"))
						(action-breakpoint brk_id file (str-to-num line)))))
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
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
								(defq word (get :text (elem-get (. match_flow :children) match_index)))
								(if (= key +char_space) (setq word (cat word " ")))
								(clear-matches)
								(dispatch-action action-select-word)
								(dispatch-action action-insert word))
							((select-match (if (= key 0x40000052) -1 1)))))
					((bits? mod +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta)
						;call bound control/command key action
						(when (defq action (. *key_map_control* :find key))
							(clear-matches)
							(dispatch-action action)))
					((bits? mod +ev_key_mod_shift)
						;call bound shift key action, else insert
						(cond
							((defq action (. *key_map_shift* :find key))
								(clear-matches)
								(dispatch-action action))
							((<= +char_space key +char_tilde)
								(dispatch-action action-insert (char key))
								(show-matches))))
					((defq action (. *key_map* :find key))
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
	(action-save-all)
	(clear-matches)
	(gui-sub-rpc *window*)
	(mail-forget edit_service)
	(profile-report "Editor"))
