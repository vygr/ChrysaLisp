;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-minimise ()
	(vdu-resize +vdu_min_width +vdu_min_height))

(defun action-maximise ()
	(vdu-resize +vdu_max_width +vdu_max_height))

(defun action-close ()
	(setq *running* nil))

(defun action-layout ()
	;user window resize
	(window-resize))

(defun action-xscroll ()
	;user xscroll bar
	(setq *scroll_x* (get :value *xslider*))
	(load-display))

(defun action-yscroll ()
	;user yscroll bar
	(setq *scroll_y* (get :value *yslider*))
	(load-display))

(defun action-tree-action ()
	;any tree mutate action
	(.-> (penv (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
		:layout :dirty_all))

(defun action-file-leaf-action ()
	;load up the file selected
	(. *open_tree* :add_route (defq file (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(select-node file)
	(populate-vdu file))

(defun action-open-leaf-action ()
	;switch to the file selected
	(. *file_tree* :add_route (defq file (. *open_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(select-node file)
	(populate-vdu file))
