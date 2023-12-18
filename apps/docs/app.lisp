(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/text/syntax.inc")

(enums +event 0
	(enum close max min)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum file_tree_collapse file_tree_expand))

(enums +select 0
	(enum main tip embeded))

(defq +margin_width (* 8 3)
	+doc_font (first (font-info *env_window_font*))
	+term_font (first (font-info *env_terminal_font*)))

;lisp handler environment
((# (def (penv) '*handler_env* (env))))

(ui-window *window* (:color +argb_grey15)
	(ui-title-bar _ "Docs" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow *doc_flow* (:flow_flags +flow_right_fill :color *env_toolbar_col*)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-flow _  (:flow_flags +flow_down_fill)
				(ui-title-bar _ "Project" (0xe940 0xe941) +event_file_tree_collapse)
				(ui-scroll *file_tree_scroll* +scroll_flag_vertical :nil
					(. (ui-tree *file_tree* +event_file_folder_action
							(:min_width 0 :color +argb_white
							:font *env_medium_terminal_font*))
						:connect +event_tree_action)))
			(ui-backdrop _ (:color +argb_white)))
		(ui-scroll *page_scroll* +scroll_flag_both (:min_height 900))))

(defun handler-func (state)
	(unless (defq handler (. handlers :find state))
		(defq module (cat "apps/docs/" (rest state) ".inc"))
		(repl (file-stream module) module)
		(. handlers :insert state handler))
	handler)

(defun populate-page (file)
	(when file
		;min width of an 80 column terminal !
		(def (defq vdu (Vdu))
			:font (create-font +term_font (page-scale 16))
			:vdu_width 80 :vdu_height 1)
		(ui-root page_flow (Flow) (:flow_flags +flow_right_fill
				:font (create-font +doc_font (page-scale 18))
				:color (get :color *window*))
			(ui-label _ (:min_width +margin_width))
			(ui-flow page (:flow_flags +flow_down_fill
					:min_width (first (. vdu :pref_size))))
			(ui-label _ (:min_width +margin_width)))
		(defq state :text)
		(each-line (lambda (line)
				(task-slice)
				(catch (setq state ((handler-func state)
							state page (trim-end line (ascii-char 13))))
					(progn (prin _) (print) (setq state :text) :t)))
			(file-stream file))
		(catch ((handler-func state) state page "")
			(progn (prin _) (print) (setq state :text) :t))
		(bind '(w h) (. page_flow :pref_size))
		(. page_flow :change 0 0 w h)
		(def *page_scroll* :min_width w)
		(def (get :vslider *page_scroll*)
			:value (if (defq pos (. scroll_pos :find file)) pos 0))
		(.-> *page_scroll* (:add_child page_flow) :layout)
		(.-> *doc_flow* :layout :dirty_all)))

(defun select-node (file)
	;highlight the selected file
	(. *file_tree* :select file)
	(bind '(w h) (. *file_tree* :pref_size))
	(. *file_tree* :change 0 0 w h)
	(def *file_tree* :min_width w)
	(def *file_tree_scroll* :min_width w)
	(.-> *file_tree_scroll* :layout :dirty_all))

(defun page-scale (s)
	(n2i (* (n2f s) *page_scale*)))

;import actions
(import "./actions.inc")

(defun main ()
	(defq select (alloc-select +select_size) syntax (Syntax) handlers (Emap)
		scroll_pos (Fmap) *running* :t *current_file* "docs/vm/vp_vm.md"
		*page_scale* 1.0)
	(. *file_tree* :populate "docs" '(".md"))
	(populate-page *current_file*)
	(select-node *current_file*)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(while *running*
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_embeded)
				;embeded event, only process internal events, not actions
				;and don't allow window events
				(and (neg? (defq id (getf *msg* +ev_msg_target_id)))
					(not (Window? (defq view (. *window* :find_id id))))
					(not (Title? view))
					(. *window* :event *msg*)))
			;must be +select_main
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key) mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta))))
						;call bound control/command key action
						(if (defq action (. *key_map_control* :find key))
							(action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action
						(if (defq action (. *key_map_shift* :find key))
							(action)))
					((defq action (. *key_map* :find key))
						;call bound key action
						(action))))
			(:t (. *window* :event *msg*)
				;save scroll position
				(. scroll_pos :insert *current_file* (get :value (get :vslider *page_scroll*))))))
	(undef (penv) '*handler_env*)
	(gui-sub *window*)
	(free-select select))
