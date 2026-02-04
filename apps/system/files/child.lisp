(import "usr/env.inc")
(import "gui/lisp.inc")

;our UI widgets and events
(import "./widgets.inc")

(defun populate-files (root exts)
	(setq exts (split exts " "))
	(if (empty? exts) (setq exts :nil))
	(.-> *file_selector* :empty (:populate root exts 2))
	(def *file_selector* :min_width 256 :min_height 512))

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun main ()
	;read parameters from parent
	(defq msg (mail-read (task-mbox)) reply_mbox (slice msg 0 +net_id_size))
	(bind '(title root &optional exts) (split (slice msg +net_id_size -1) "|"))
	(setd exts "")
	(def *window_title* :text title)
	(defq *running* :t *current_root* root *current_exts* exts)
	(. *exts* :set_text exts)
	(populate-files *current_root* *current_exts*)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(while *running*
		(defq *msg* (mail-read (task-mbox)))
		(cond
			((eql *msg* "")
				(setq *running* :nil))
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(action))
			(:t ;gui event
				(. *window* :event *msg*))))
	(gui-sub-rpc *window*))
