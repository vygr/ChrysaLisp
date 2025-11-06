(import "././login/env.inc")
(import "gui/lisp.inc")

;our config UI widgets
(import "./config.inc")
(import "./widgets.inc")

(enums +select 0
	(enum main tip))

(defun layout-items ()
	(defq item_flows (list *todo_flow* *done_flow* *deleted_flow*)
		scrolls (list *todo_scroll* *done_scroll* *deleted_scroll*)
		item_backs (list *todo_back* *done_back* *deleted_back*)
		scroll_childs (list *todo_scroll_child* *done_scroll_child* *deleted_scroll_child*))
	(each (lambda (scroll scroll_child)
			(bind '(w h) (. scroll_child :pref_size))
			(def scroll :min_width w :min_height h))
		scrolls scroll_childs)
	(bind '(w h) (. (get :stack_flow *stack_flow*) :pref_size))
	(bind '(_ s) (. (some (# (first (. %0 :children))) item_flows) :pref_size))
	(each (lambda (item_back scroll_child scroll)
			(. scroll_child :change_dirty 0 0 w h :t)
			(def item_back :spacing s)
			(def scroll :min_width w :min_height (min 512 h)))
		item_backs scroll_childs scrolls))

(defun populate-items ()
	(defq item_lists (gather *config* :todo :done :deleted)
		item_flows (list *todo_flow* *done_flow* *deleted_flow*)
		scrolls (list *todo_scroll* *done_scroll* *deleted_scroll*)
		item_backs (list *todo_back* *done_back* *deleted_back*)
		scroll_childs (list *todo_scroll_child* *done_scroll_child* *deleted_scroll_child*))
	(each (lambda (scroll scroll_child item_flow item_list)
			(each (lambda (item)
				(ui-root entry (Flow) (:flow_flags +flow_right_fill)
					(ui-tool-bar toolbar (:font *env_small_toolbar_font*)
						(ui-buttons (0xe94e 0xe94d 0xe94c) +event_done))
					(ui-textfield _ (:clear_text item :color +argb_white)))
				(. item_flow :add_child entry)
				(ui-tool-tips toolbar '("done it" "redo it" "delete"))) item_list))
		scrolls scroll_childs item_flows item_lists))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t
		todo_service (mail-declare (task-mbox) "Todo" "Todo Service 0.1"))
	(load-config)
	(populate-items)
	(layout-items)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key)
					mod (getf *msg* +ev_msg_key_mod))
				(cond
					((bits? mod +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta)
						;call bound control/command key action
						(when (defq action (. *key_map_control* :find key))
							(dispatch-action action)))
					((bits? mod +ev_key_mod_shift)
						;call bound shift key action, else insert
						(cond
							((defq action (. *key_map_shift* :find key))
								(dispatch-action action))
							((<= +char_space key +char_tilde)
								;insert char etc ...
								(char key))))
					((defq action (. *key_map* :find key))
						;call bound key action
						(dispatch-action action))
					((<= +char_space key +char_tilde)
						;insert char etc ...
						(char key))))
			(:t ;gui event
				(. *window* :event *msg*))))
	(save-config)
	(gui-sub-rpc *window*)
	(mail-forget todo_service))
