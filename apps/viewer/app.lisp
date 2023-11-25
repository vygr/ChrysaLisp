(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/consts/chars.inc")
(import "lib/text/buffer.inc")
(import "././clipboard/app.inc")

(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum copy paragraph block bracket_left bracket_right)
	(enum whole_words regexp find_down find_up)
	(enum file_tree_collapse file_tree_expand))

(enums +select 0
	(enum main tip))

(bind '(+edit_font +edit_size) (font-info *env_editor_font*))

(defq +vdu_min_width 40 +vdu_min_height 20 +vdu_max_width 100
	+vdu_max_height 46 +vdu_line_width 5 +margin 2 +status_min_size 32
	+text_types ''(".md" ".txt") +file_types ''(".lisp" ".inc" ".vp" ".md" ".txt" ".tre"))

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "Viewer" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe9c9 0xe90d 0xe955 0xe93c 0xe93d) +event_copy))
		(ui-tool-bar *find_toolbar* (:color (const *env_toolbar2_col*))
			(ui-buttons (0xe9cd 0xe9a8 0xe914 0xe91b) +event_whole_words))
		(. (ui-textfield *find_text* (:color +argb_white
				:hint_text "find" :clear_text "")) :connect +event_find_down))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-flow _  (:flow_flags +flow_down_fill :color +argb_grey14)
				(ui-flow _ (:flow_flags +flow_left_fill)
					(ui-buttons (">" "^") +event_file_tree_collapse)
					(ui-label _ (:text "Project")))
				(ui-scroll *file_tree_scroll* +scroll_flag_vertical :nil
					(. (ui-tree *file_tree* +event_file_folder_action
							(:min_width 0 :color +argb_white
							:font *env_medium_terminal_font*)) :connect +event_tree_action)))
			(ui-backdrop _ (:color +argb_white)))
		(ui-flow _ (:flow_flags +flow_up_fill)
			(ui-flow _ (:flow_flags +flow_stack_fill :color +argb_white
					:font *env_medium_terminal_font*)
				(ui-flow _ (:flow_flags +flow_right)
					(ui-text _ (:text "cx: ")) (ui-text *cx* (:min_width +status_min_size))
					(ui-text _ (:text "cy: ")) (ui-text *cy* (:min_width +status_min_size))
					(ui-text _ (:text "sw: ")) (ui-text *sw* (:min_width +status_min_size))
					(ui-text _ (:text "sh: ")) (ui-text *sh* (:min_width +status_min_size))
					(ui-text _ (:text "fc: ")) (ui-text *fc* (:min_width +status_min_size)))
				(ui-backdrop _))
			(ui-flow *scale_flow* (:flow_flags +flow_right_fill)
				(ui-vdu *vdu_lines* (:min_width +vdu_line_width :min_height 0
						:vdu_width +vdu_line_width :vdu_height +vdu_min_height
						:ink_color +argb_grey12 :font *env_editor_font*))
				(ui-backdrop _ (:color (get :ink_color *vdu_lines*) :min_width 1))
				(ui-flow _ (:flow_flags +flow_left_fill)
					(. (ui-slider *yslider*) :connect +event_yscroll)
					(ui-flow *edit_flow* (:flow_flags +flow_up_fill)
						(. (ui-slider *xslider*) :connect +event_xscroll)))))))

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
	(.-> buffer (:vdu_load (. *edit* :get_vdu_text) sx sy)
		(:find (. *find_text* :get_text) *whole_words* *regexp*))
	(.-> *edit* :underlay_paper :underlay_ink)
	(defq fc (reduce (# (+ %0 (if %1 (length (first %1)) 0)))
		(. buffer :get_found) 0))
	;update status bar
	(each (# (def (. %0 :dirty) :text (str %1)))
		(list *cx* *cy* *sw* *sh* *fc*)
		(list (inc cx) (inc cy) (abs (- cx ax)) (abs (- cy ay)) fc)))

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

(defun populate-buffer (file cx cy ax ay sx sy)
	;create new file buffer ?
	(defq mode (notany (# (ends-with %0 file)) +text_types)
		files (. *meta_map* :find :files) key (str file))
	(unless (. files :find key)
		(. files :insert key
			(Fmap-kv :cx cx :cy cy :ax ax :ay ay :sx sx :sy sy :buffer :nil)))
	(defq meta (. files :find key))
	(unless (defq buffer (. meta :find :buffer))
		(. meta :insert :buffer (setq buffer (Buffer mode *syntax*))))
	(when file (. buffer :file_load file)))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(populate-buffer file 0 0 0 0 0 0)
	(defq meta (.-> *meta_map* (:find :files) (:find (str file))))
	(bind '(cx cy ax ay sx sy buffer)
		(gather meta :cx :cy :ax :ay :sx :sy :buffer))
	(setq *current_file* file)
	(bind '(cx cy) (. buffer :constrain cx cy))
	(bind '(ax ay) (. buffer :constrain ax ay))
	(bind '(sx sy) (. buffer :constrain sx sy))
	(.-> *edit* (:set_buffer buffer)
		(:set_cursor cx cy)
		(:set_anchor ax ay)
		(:set_scroll sx sy))
	(scatter meta :cx cx :cy cy :ax ax :ay ay :sx sx :sy sy)
	(refresh)
	(def *title* :text (cat "Viewer" (if file (cat " -> " file) "")))
	(.-> *title* :layout :dirty))

(defun populate-file-trees ()
	;refresh file tree
	(bind '(w h) (. *file_tree* :pref_size))
	(def *file_tree* :min_width w)
	(def *file_tree_scroll* :min_width w)
	(.-> *file_tree* (:change 0 0 w h) :layout)
	(bind '(w h) (. *file_tree* :pref_size))
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

(defun select-node (file)
	;highlight selected file
	(. *file_tree* :select file))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip select))
	(ui-tool-tips *main_toolbar*
		'("copy" "select paragraph" "select form" "start form" "end form"))
	(ui-tool-tips *find_toolbar*
		'("whole words" "regexp" "find down" "find up")))

(defun page-scale (s)
	(n2i (* (n2f s) *page_scale*)))

(defun update-meta-data ()
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy) (. *edit* :get_cursor))
	(bind '(ax ay) (. *edit* :get_anchor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(scatter *meta_map*
		:file (str *current_file*)
		:find (. *find_text* :get_text))
	(scatter (.-> *meta_map* (:find :files) (:find (str *current_file*)))
		:cx cx :cy cy :ax ax :ay ay :sx sx :sy sy :buffer buffer))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(defq func (first action))
	(if (find func *find_actions*)
		(push action *whole_words* *regexp* (. *find_text* :get_text)))
	(catch (eval action)
		(progn (print _)(print)
			(setq *refresh_mode* (list 0)) :t)))

(defun main ()
	(defq select (alloc-select +select_size)
		*running* :t *edit* (Viewer-edit) *page_scale* 1.0 *regexp* :nil
		*syntax* (Syntax) *whole_words* :nil *refresh_mode* (list 0)
		*meta_map* (Fmap-kv :files (Fmap)) *current_file* :nil)
	(.-> *edit* (:set_buffer (Buffer))
		(:set_select_color +argb_grey6)
		(:set_found_color +argb_grey4)
		(:set_region_color +argb_grey3))
	(def *edit* :min_width 0 :min_height 0
		:vdu_width +vdu_min_width :vdu_height +vdu_min_height)
	(. *edit_flow* :add_back *edit*)
	(. *file_tree* :populate "." +file_types 2)
	(populate-file-trees)
	(populate-vdu *current_file*)
	(tooltips)
	(action-minimise)
	(bind '(x y w h) (apply view-locate (.-> *window* (:connect +event_layout) :get_size)))
	(gui-add-front (. *window* :change x y w h))
	(refresh)
	(while *running*
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
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
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta))))
						;call bound control/command key action
						(when (defq action (. *key_map_control* :find key))
							(dispatch-action action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action
						(cond
							((defq action (. *key_map_shift* :find key))
								(dispatch-action action))))
					((defq action (. *key_map* :find key))
						;call bound key action
						(dispatch-action action))
					))
			(:t ;gui event
				(. *window* :event *msg*)))
		;update meta data
		(update-meta-data))
	(free-select select)
	(gui-sub *window*))
