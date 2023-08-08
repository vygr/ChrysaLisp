;(import "lib/debug/frames.inc")

(import "././login/env.inc")
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/text/syntax.inc")

(enums +event 0
	(enum close max min)
	(enum tree_action)
	(enum file_folder_action file_leaf_action))

(enums +select 0
	(enum main tip))

(defq +margin_width (* 8 3) syntax (Syntax) handlers (emap) scroll_pos (fmap) *current_file* :nil *selected_file_node* :nil
*running* :t)

(defun handler-func (state)
	(unless (defq handler (. handlers :find state))
		(defq module (cat "apps/docs/" (slice 1 -1 state) ".inc"))
		(repl (file-stream module) module)
		(. handlers :insert state handler))
	handler)

(defun populate-page (file)
	(when file
		(ui-root page_flow (Flow) (:flow_flags +flow_right_fill :font *env_window_font*
				:color (get :color *window*))
			(ui-label _ (:min_width +margin_width))
			(ui-flow page (:flow_flags +flow_down_fill :min_width 800))
			(ui-label _ (:min_width +margin_width)))
		(defq state :text)
		(each-line (lambda (line)
				(task-slice)
				(setq state ((handler-func state) state page (trim-end line (ascii-char 13)))))
			(file-stream file))
		((handler-func state) state page "")
		(bind '(w h) (. page_flow :pref_size))
		(. page_flow :change 0 0 w h)
		(def page_scroll :min_width w)
		(def (get :vslider page_scroll) :value (if (defq pos (. scroll_pos :find file)) pos 0))
		(.-> page_scroll (:add_child page_flow) (:layout))
		(.-> doc_flow :layout :dirty_all)))

(ui-window *window* (:color +argb_grey15)
	(ui-title-bar _ "Docs" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow doc_flow (:flow_flags +flow_right_fill :font *env_window_font* :color *env_toolbar_col*)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-scroll *file_tree_scroll* +scroll_flag_vertical :nil
				(. (ui-tree *file_tree* +event_file_folder_action
						(:min_width 0 :color +argb_white :font *env_medium_terminal_font*))
					:connect +event_tree_action))
			(ui-backdrop _ (:color +argb_white)))
		(ui-scroll page_scroll +scroll_flag_vertical (:min_height 900))))

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

;import actions
(import "./actions.inc")

(defun main ()
	(defq select (alloc-select +select_size) *running* :t)
	(. *file_tree* :populate "docs" '(".md"))
	(populate-page "docs/INTRO.md")
	(select-node :nil)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))

	(while *running* 
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((defq id (getf *msg* +ev_msg_target_id) action (. event_map :find id))
				;call bound event action
				(action))
			(:t (. *window* :event *msg*))))

	(gui-sub *window*)
	(free-select select))
