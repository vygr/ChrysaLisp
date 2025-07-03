(import "././login/env.inc")
(import "gui/lisp.inc")

;our UI widgets and events
(import "./widgets.inc")

(defun populate-files (root exts)
	(setq exts (split exts " "))
	(if (empty? exts) (setq exts :nil))
	(.-> *file_tree* :empty (:populate root exts 2))
	(bind '(w h) (. *file_tree* :pref_size))
	(. *file_tree* :change 0 0 w h)
	(defq w 400 h 400)
	(def *file_tree_scroll* :min_width w :min_height h)
	(.-> *file_tree_scroll* :layout :dirty_all))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun main ()
	;read parameters from parent
	(bind '(reply_mbox title root exts) (mail-read (task-netid)))
	(def *window_title* :text title)
	(defq *running* :t *current_root* root *current_exts* exts)
	(. *exts* :set_text exts)
	(populate-files *current_root* *current_exts*)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while *running*
		(defq *msg* (mail-read (task-netid)))
		(cond
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(action))
			(:t ;gui event
				(. *window* :event *msg*))))
	(gui-sub-rpc *window*))
